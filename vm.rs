#![feature(alloc_system)]

extern crate alloc_system;

use std::env;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Read;
use std::io::Write;
use std::io;
use std::process;
use std::str::FromStr;

// Here's how I did it:
//
// # V1
//
// First version was just capable of running the program and nothing else.
// I realized my first mistake in a few minutes: There was no way of saving the
// game so if I've been eaten by a grue or something I'd have to start from
// scratch.
//
// Second mistake was running an optimized build (or some kind of build with
// less debugging info than I needed). When I first made it to the "office", I
// wanted to fire up GDB and look at the VM's registers. Note that at this point
// I couldn't exit because that that would mean losing all the progress. GDB
// couldn't even list locals in my stack frames, so I had no choice but to start
// from scratch with a modified VM.
//
// (maybe talk a little bit aobut how I solved the equation on the wall)
//
// # V2
//
// First, the VM now saves all the input to a file. Second, it takes a command
// line argument for a file path and reads that file into the input buffer so
// that when the program runs, characters are read from that pre-loaded buffer.
// Assuming no random events occur in the game, this should be enough for
// save/restore. It's actually much more flexible than traditional save/load
// systems as you can modify the input file or trim it to load to different
// points.
//
// # V3
//
// Still can't use the teleporter. I added a spacial case for when input is
// "/record". Upon seeing that input, VM starts to record the execution to
// stderr. I see this line in the execution record:
//
//   Executing instruction: Jf [Reg(7), Num(5605)]
//   Regs: [2708, 5445, 3, 10, 101, 0, 0, 0]
//   Stack: [6080, 16, 6124, 1, 2952, 25978, 3568, 3599, 2708, 5445, 3]
//
// The "strange book" was talking about register 8. This should be it. But it's
// not clear 1) what value should be written in register 8 2) how to by-pass the
// validation...
//
// If I set it something non-zero, I get this:
//
//   A strange, electronic voice is projected into your mind:
//
//     "Unusual setting detected!  Starting confirmation process!  Estimated
//     time to completion: 1 billion years."
//
// Just for the amusements, I left my laptop running this process. After a while
// it printed this:
//
//   A strange, electronic voice is projected into your mind:
//
//     "Miscalibration detected!  Aborting teleportation!"
//
//   Nothing else seems to happen.
//
// So maybe, if I can find the correct number, I don't need to by-pass the
// verification process. Brute-force is definitely not possible though, this
// took about 10 minutes. 2^15 numbers, each one taking 10 mintes to take, would
// take 227 days to test.
//
// ...
//
// Started implementing a debugger. Not sure how I'll use it but I'm hoping to
// inspect which instructions read/ write to R8.
//
// I just realized things would be so much easier if we knew that the program
// isn't generating any instructions dynamically...
//
// ...
//
// Some investigation on the execution using new `break_reg_read` and
// `break_reg_write` breakpoints revealed that when teleporter is used, R8 is
// compared with zero:
//
//   > Jf [Reg(7), Num(5605)]
//
// (reg indices start from 0, so Reg(7) is actually register 8)
//
// Next obvious step is to disassemble instructions after this instruction and
// to try to understand what's happening. For that I'll first need to implement
// a disassembler.

////////////////////////////////////////////////////////////////////////////////

// A problem with higher-order functions like `or_else()` is that you can't
// `continue` (or `break`) a loop using from the lambdas passed to them. In a
// language with TCO, I'd just call the loop function. In Rust I guess I do
// this:

macro_rules! unwrap_or_cont_res {
    ($x:expr) => {
        match $x {
            Ok(x) => x,
            Err(err) => {
                println!("ERROR: {}", err);
                continue;
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Value {
    /// 0 .. 32767
    Num(u16),

    /// Values 32768 .. 32775 mean registers 0..7
    Reg(u8)

    // 32776 .. 65535 are invalid
}

impl Value {
    fn decode(val : u16) -> Value {
        if val <= 32767 {
            Value::Num(val)
        } else if val <= 32775 {
            Value::Reg((val - 32768) as u8)
        } else {
            panic!("Valude::decode: Invalid value: {}", val)
        }
    }

    fn reg(&self) -> u8 {
        match *self {
            Value::Num(_) => {
                panic!("Value::reg: Found num: {:?}", self)
            },
            Value::Reg(r) => {
                r
            }
        }
    }
}

/// 2^15
const ADDRESS_SPACE : usize = 32768;

/// Virtual machine state.
struct VM {
    /// 8 registers.
    regs:  [u16; 8],

    /// Unbounded stack of 16-bit values.
    stack: Vec<u16>,

    /// Memory is addressed using 15-bit addresses. Each cell holds 16-bit
    /// value. 64Kb in total.
    mem:   [u16; ADDRESS_SPACE],

    /// Instruction pointer. Actually 15-bit.
    ip:    u16,

    input_buf : String,

    /// Input is saved here. This should be enough (assuming no random events
    /// occur in the game) to save/restore game state. Hopefully the assumption
    /// is correct.
    input_log_file : Option<File>,

    breakpoints : Vec<Breakpoint>,
}

#[derive(Clone, Copy, Debug)]
enum Breakpoint {
    /// Break when instruction pointer is pointing at the given address.
    Addr { addr: u16 },

    /// Break when instruction pointer is pointing at a given type of
    /// instruction.
    Instr { instr: Instr },

    /// Break when hit 0. NOTE: Doesn't count print instructions.
    Count(i32),

    /// Break when a register is read.
    RegRead { reg: u8 },

    /// Break when a register is written.
    RegWrite { reg: u8 },
}

impl Breakpoint {
    fn ticks_left(&self) -> bool {
        match *self {
            Breakpoint::Count(n) => {
                if n < 0 { false } else { true }
            }
            _ => true,
        }
    }

    fn tick(&mut self) {
        match self {
            &mut Breakpoint::Count(ref mut n) => {
                *n = *n - 1;
            },
            _ => {}
        }
    }
}

impl VM {
    fn init() -> VM {
        VM {
            regs: [0; 8],
            stack: Vec::new(),
            mem: [0; ADDRESS_SPACE],
            ip: 0,
            input_buf: String::new(),
            input_log_file: None,
            breakpoints: Vec::new(),
        }
    }

    fn init_with_logging(log_file : &str) -> VM {
        let mut vm = VM::init();
        vm.load_pgm("challenge.bin");

        vm.input_log_file =
            Some(OpenOptions::new()
                 .create(true)
                 .read(true)
                 .append(true)
                 .open(log_file).unwrap());

        let mut input = String::new();
        vm.input_log_file.as_ref().unwrap().read_to_string(&mut input).unwrap();
        vm.load_input(input);

        vm
    }

    fn load_pgm(&mut self, path : &str) {
        let mut low_byte = true;
        let mut num : u16 = 0;
        let mut mem_addr = 0;
        for byte in File::open(path).unwrap().bytes() {
            if low_byte {
                num = byte.unwrap() as u16;
                low_byte = false;
            } else {
                num += (byte.unwrap() as u16) << 8;
                self.mem[mem_addr] = num;
                mem_addr += 1;
                low_byte = true;
            }
        }

        println!("{} values loaded.", mem_addr);
    }

    fn load_input(&mut self, input : String) {
        self.input_buf = input;
    }

    fn run (&mut self) {
        loop {
            if self.execute_instr() {
                break;
            }
        }
    }

    fn decode_instr_at(&self, ip : u16) -> Option<(Instr, Vec<Value>)> {
        assert!(ip < 32758);

        let opcode = self.mem[ip as usize];

        if opcode > 21 {
            return None;
        }
        // assert!(opcode <= 21, "Invalid opcode at addr {}: {}", ip, opcode);

        let opcode_args = OPCODE_INSTRS[opcode as usize].1;
        let mut args = Vec::with_capacity(opcode_args as usize);

        for i in 0 .. opcode_args {
            let idx = ((ip + 1) as usize) + (i as usize);
            let val = self.mem[idx];
            args.push(Value::decode(val));
        }

        Some((OPCODE_INSTRS[opcode as usize].0, args))
    }

    fn num(&self, val : Value) -> u16 {
        match val {
            Value::Num(n) => n,
            Value::Reg(r) => self.regs[r as usize],
        }
    }

    /// True -> halt
    fn execute_instr(&mut self) -> bool {
        let mut brk = false;

        if self.ip == 6027 {
            println!("Ackermann function called.");
            self.show_vm_state();
            println!("\nReturning without running the function...");
            let ret_addr = self.stack.pop().unwrap();
            self.ip = ret_addr;
            brk = true;
        }

        let (instr, args) = self.decode_instr_at(self.ip).unwrap();

        // Skip instructions don't tick breakpoints. (so `break_instr out` won't
        // work but that's OK)
        if instr != Instr::Out {
            self.breakpoints.retain(|b| b.ticks_left());

            for bp in self.breakpoints.iter_mut() {
                bp.tick();

                if !brk {
                    match *bp {
                        Breakpoint::Addr { addr } => {
                            brk = brk || addr == self.ip;
                        },
                        Breakpoint::Instr { instr: instr1 } => {
                            brk = brk || instr == instr1;
                        },
                        Breakpoint::Count(n) => {
                            brk = brk || n == 0;
                        },
                        Breakpoint::RegRead { reg } => {
                            let reg_val = Value::Reg(reg);
                            match instr {
                                Instr::Set => {
                                    brk = brk || args[1] == reg_val;
                                },
                                Instr::Push => {
                                    brk = brk || args[0] == reg_val;
                                },
                                Instr::Eq => {
                                    brk = brk || args[0] == reg_val || args[1] == reg_val;
                                },
                                Instr::Gt => {
                                    brk = brk || args[1] == reg_val || args[2] == reg_val;
                                },
                                Instr::Jt | Instr::Jf | Instr::Call | Instr::Jmp => {
                                    brk = brk || args[0] == reg_val;
                                },
                                Instr::Add | Instr::Mult | Instr::Mod | Instr::And | Instr::Or => {
                                    brk = brk || args[1] == reg_val || args[2] == reg_val;
                                },
                                Instr::Not => {
                                    brk = brk || args[1] == reg_val;
                                },
                                Instr::Rmem | Instr::Wmem => {
                                    brk = brk || args[1] == reg_val;
                                },
                                Instr::Halt | Instr::Pop | Instr::Ret | Instr::Out | Instr::In | Instr::Noop => {}
                            }
                        },
                        Breakpoint::RegWrite { reg } => {
                            let reg_val = Value::Reg(reg);
                            match instr {
                                Instr::Set => {
                                    brk = brk || args[1] == reg_val;
                                },
                                Instr::Pop => {
                                    brk = brk || args[0] == reg_val;
                                },
                                Instr::Eq | Instr::Gt | Instr::Add | Instr::Mult | Instr::Mod
                                    | Instr::And | Instr::Or | Instr::Not | Instr::Rmem | Instr::Wmem  =>
                                {
                                    brk = brk || args[0] == reg_val;
                                },
                                Instr::Push | Instr::Jmp | Instr::Jt | Instr::Jf | Instr::Halt
                                    | Instr::Call | Instr::Ret | Instr::Out | Instr::In | Instr::Noop => {}
                            }
                        },
                    }
                }
            }

            if brk {
                return self.show_debug_prompt(instr, args);
            }
        }

        self.execute_instr_(instr, args)
    }

    fn show_vm_state(&self) {
        println!("");
        println!("REGS:       [{}] [{}] [{}] [{}] [{}] [{}] [{}] [{}]",
                 self.regs[0], self.regs[1], self.regs[2], self.regs[3],
                 self.regs[4], self.regs[5], self.regs[6], self.regs[7]);
        println!("STACK:      {:?}", self.stack);
        println!("IP:         {}", self.ip);
        // println!("INSTR SIZE: {}", instr.len());
        // println!("NEXT IP:    {}", self.ip + instr.len());
    }

    fn show_debug_prompt(&mut self, instr : Instr, args : Vec<Value>) -> bool {
        self.show_vm_state();
        println!(">>> {:?} {:?}\n", instr, args);

        loop {
            print!("> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            let words : Vec<&str> = input.split_whitespace().collect();

            if words.len() == 0 || words[0] == "step" {
                self.breakpoints.push(Breakpoint::Count(1));
                return self.execute_instr_(instr, args);
            }

            else if words[0] == "status" {
                self.show_vm_state();
            }

            else if words[0] == "c" {
                // let (instr, args) = self.decode_instr_at(self.ip).unwrap();
                // return self.execute_instr_(instr, args);
                return self.execute_instr();
            }

            else if words[0] == "skip" {
                self.breakpoints.push(Breakpoint::Count(unwrap_or_cont_res!(words[1].parse())));
                return self.execute_instr_(instr, args);
            }

            else if words[0] == "break_addr" {
                self.breakpoints.push(
                    Breakpoint::Addr { addr: unwrap_or_cont_res!(words[1].parse()) });
            }

            else if words[0] == "break_instr" {
                self.breakpoints.push(
                    Breakpoint::Instr { instr: unwrap_or_cont_res!(words[1].parse()) });
            }

            else if words[0] == "break_reg_read" {
                self.breakpoints.push(
                    Breakpoint::RegRead { reg: unwrap_or_cont_res!(words[1].parse()) });
            }

            else if words[0] == "break_reg_write" {
                self.breakpoints.push(
                    Breakpoint::RegWrite { reg: unwrap_or_cont_res!(words[1].parse()) });
            }

            else if words[0] == "set_reg" {
                // let reg_no : usize = unwrap_or_cont_res!(words[1].parse());
                let reg_no : usize = unwrap_or_cont_res!(words[1].parse());
                let val = unwrap_or_cont_res!(words[2].parse());
                self.regs[reg_no] = val;
            }

            else if words[0] == "set_ip" {
                let ip = unwrap_or_cont_res!(words[1].parse());
                self.ip = ip;
            }

            else if words[0] == "reg" {
                let reg_no : usize = unwrap_or_cont_res!(words[1].parse());
                let reg_val = self.regs[reg_no];
                println!("REG {}: {}", reg_no, reg_val);
            }

            else if words[0] == "mem" {
                let addr : usize = unwrap_or_cont_res!(words[1].parse());
                println!("MEM {}: {}", addr, self.mem[addr]);
            }

            else if words[0] == "breakpoints" {
                self.show_breakpoints();
            }

            else if words[0] == "remove_bp" {
                let bp = unwrap_or_cont_res!(words[1].parse());
                self.breakpoints.remove(bp);
            }

            else if words[0] == "disas" {
                let mut addr : u16 = unwrap_or_cont_res!(words[1].parse());
                let instrs : u16 = unwrap_or_cont_res!(words[2].parse());
                for _ in 0 .. instrs {
                    match self.decode_instr_at(addr) {
                        None => { break; }
                        Some((instr, args)) => {
                            println!("[{}] {:?} {:?}", addr, instr, args);
                            addr = addr + instr.len();
                        }
                    }
                }
            }

            else {
                println!("Unknown command: {}", input);
            }
        }
    }

    fn execute_instr_(&mut self, instr : Instr, args : Vec<Value>) -> bool {
        assert_eq!(OPCODE_INSTRS[instr as usize].1 as usize, args.len());

        match instr {
            Instr::Halt => {
                true
            },

            Instr::Set => {
                let reg = args[0].reg();
                let val = self.num(args[1]);
                self.regs[reg as usize] = val;
                self.ip += instr.len();
                false
            },

            Instr::Push => {
                let val = self.num(args[0]);
                self.stack.push(val);
                self.ip += instr.len();
                false
            },

            Instr::Pop => {
                let top = self.stack.pop().unwrap();
                self.regs[args[0].reg() as usize] = top;
                self.ip += instr.len();
                false
            },

            Instr::Eq => {
                let reg  = args[0].reg();
                let val1 = self.num(args[1]);
                let val2 = self.num(args[2]);
                self.regs[reg as usize] = if val1 == val2 { 1 } else { 0 };
                self.ip += instr.len();
                false
            },

            Instr::Gt => {
                let reg  = args[0].reg();
                let val1 = self.num(args[1]);
                let val2 = self.num(args[2]);
                self.regs[reg as usize] = if val1 > val2 { 1 } else { 0 };
                self.ip += instr.len();
                false
            },

            Instr::Jmp => {
                self.ip = self.num(args[0]);
                false
            },

            Instr::Jt => {
                if self.num(args[0]) != 0 {
                    self.ip = self.num(args[1]);
                } else {
                    self.ip += instr.len();
                }
                false
            },

            Instr::Jf => {
                if self.num(args[0]) == 0 {
                    self.ip = self.num(args[1]);
                } else {
                    self.ip += instr.len();
                }
                false
            },

            Instr::Add => {
                // We can just add numbers as two 15-bit numbers can't overflow
                // in 16-bit type.
                self.regs[args[0].reg() as usize] =
                    (self.num(args[1]) + self.num(args[2])) % 32768;
                self.ip += instr.len();
                false
            },

            Instr::Mult => {
                let num1 = self.num(args[1]) as u32;
                let num2 = self.num(args[2]) as u32;

                self.regs[args[0].reg() as usize] =
                    ((num1 * num2) % 32768) as u16;
                self.ip += instr.len();
                false
            },

            Instr::Mod => {
                self.regs[args[0].reg() as usize] =
                    self.num(args[1]) % self.num(args[2]);
                self.ip += instr.len();
                false
            },

            Instr::And => {
                self.regs[args[0].reg() as usize] =
                    self.num(args[1]) & self.num(args[2]);
                self.ip += instr.len();
                false
            },

            Instr::Or => {
                self.regs[args[0].reg() as usize] =
                    self.num(args[1]) | self.num(args[2]);
                self.ip += instr.len();
                false
            },

            Instr::Not => {
                self.regs[args[0].reg() as usize] =
                    (!self.num(args[1])) & 0b111111111111111;
                self.ip += instr.len();
                false
            },

            Instr::Rmem => {
                self.regs[args[0].reg() as usize] = self.mem[self.num(args[1]) as usize];
                self.ip += instr.len();
                false
            },

            Instr::Wmem => {
                self.mem[self.num(args[0]) as usize] = self.num(args[1]);
                self.ip += instr.len();
                false
            },

            Instr::Call => {
                self.stack.push(self.ip + instr.len());
                self.ip = self.num(args[0]);
                false
            },

            Instr::Ret => {
                if let Some(ip) = self.stack.pop() {
                    self.ip = ip;
                    false
                } else {
                    true
                }
            },

            Instr::Out => {
                let char = (self.num(args[0]) as u8) as char;
                print!("{}", char);
                self.ip += instr.len();
                false
            },

            Instr::In => {
                // according to the spec it's OK to read whole lines and buffer
                // to act like reading chars here
                if self.input_buf.len() == 0 {
                    io::stdin().read_line(&mut self.input_buf).unwrap();

                    if self.input_buf == "/dump\n" {
                        {
                            let mut file = OpenOptions::new()
                                .create(true)
                                .write(true)
                                .open("dump").unwrap();

                            for word in self.mem.iter() {
                                file.write(&[ *word as u8,
                                             (*word >> 8) as u8 ]).unwrap();
                            }
                        }

                        println!("Done.");

                        // Read another line
                        self.input_buf.clear();
                        return self.execute_instr_(instr, args);
                    }

                    else if self.input_buf == "/debug\n" {
                        self.input_buf.clear();
                        return self.show_debug_prompt(instr, args)
                    }

                    else {
                        if let Some(ref mut input_log_file) = self.input_log_file {
                            write!(input_log_file, "{}", self.input_buf).unwrap();
                        }
                    }
                }

                let char = self.input_buf.chars().nth(0).unwrap();
                self.input_buf.remove(0); // slow but who cares
                self.regs[args[0].reg() as usize] = (char as u8) as u16;
                self.ip += instr.len();
                false
            },

            Instr::Noop => {
                self.ip += instr.len();
                false
            }
        }
    }

    fn show_breakpoints(&self) {
        for (bp_idx, bp) in self.breakpoints.iter().enumerate() {
            println!("[{}] {:?}", bp_idx, bp);
        }
    }
}

/// Instructions. Enum value is opcode. Refer to OPCODE_INSTRS to number of
/// arguments they take.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Instr {
    Halt = 0,
    Set,
    Push, Pop,
    Eq, Gt,
    Jmp, Jt, Jf,
    Add, Mult, Mod,
    And, Or, Not,
    Rmem, Wmem,
    Call, Ret,
    Out, In,
    Noop
}

impl FromStr for Instr {
    type Err = String;

    fn from_str(s : &str) -> Result<Instr, String> {
        for instr in OPCODE_INSTRS.iter() {
            if instr.2 == s {
                return Ok(instr.0)
            }
        }

        Err(format!("No such instruction: {}", s))
    }
}

impl Instr {
    fn len(&self) -> u16 {
        OPCODE_INSTRS[*self as usize].1 as u16 + 1
    }
}

/// Mapping of instructions to number of arguments they take. Instruction at
/// index n has opcode n. Enum value of instructions also give these opcodes.
static OPCODE_INSTRS : [(Instr, u8, &'static str); 22] =
    [ (Instr::Halt, 0, "halt"),
      (Instr::Set, 2, "set"),
      (Instr::Push, 1, "push"),
      (Instr::Pop, 1, "pop"),
      (Instr::Eq, 3, "eq"),
      (Instr::Gt, 3, "gt"),
      (Instr::Jmp, 1, "jmp"),
      (Instr::Jt, 2, "jt"),
      (Instr::Jf, 2, "jf"),
      (Instr::Add, 3, "add"),
      (Instr::Mult, 3, "mult"),
      (Instr::Mod, 3, "mod"),
      (Instr::And, 3, "and"),
      (Instr::Or, 3, "or"),
      (Instr::Not, 2, "not"),
      (Instr::Rmem, 2, "rmem"),
      (Instr::Wmem, 2, "wmem"),
      (Instr::Call, 1, "call"),
      (Instr::Ret, 0, "ret"),
      (Instr::Out, 1, "out"),
      (Instr::In, 1, "in"),
      (Instr::Noop, 0, "noop") ];

fn main() {
    println!("Running a test...");
    {
        let mut test_vm = VM::init();
        test_vm.mem[0] = 9;
        test_vm.mem[1] = 32768;
        test_vm.mem[2] = 32769;
        test_vm.mem[3] = 4;
        test_vm.mem[4] = 19;
        test_vm.mem[5] = 32768;
        test_vm.regs[1] = 39;
        test_vm.run();
        assert_eq!(test_vm.regs[0], 43);
        println!("\nDone.");
    }

    let args = env::args().collect::<Vec<String>>();

    let log_file =
        if args.len() == 1 {
            "vm_input.txt"
        } else if args.len() == 2 {
            &args[1]
        } else {
            println!("USAGE: ./vm [input file]");
            process::exit(1);
        };

    let mut vm = VM::init_with_logging(log_file);
    vm.load_pgm("challenge.bin");
    vm.run();
}
