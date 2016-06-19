#![feature(alloc_system)]
extern crate alloc_system;

use std::collections::HashMap;
use std::thread;
use std::u16;

#[inline]
fn add(i1 : u16, i2 : u16) -> u16 {
    (i1 + i2) % 32768
}

#[derive(Debug)]
struct FnState {
    reg0  : u16,
    reg1  : u16,
    reg7  : u16,
    stack : Vec<u16>,
}

impl FnState {
    fn init(reg0 : u16, reg1 : u16, reg7 : u16) -> FnState {
        FnState {
            reg0: reg0,
            reg1: reg1,
            reg7: reg7,
            stack: Vec::new(),
        }
    }

    // So this is the disassembled Ackermann-like function. If we could remove
    // some dependencies between calls it'd be easier to understand/
    // parallelize/ memoize.
    fn f(&mut self) {
        if self.reg0 == 0 {
            self.reg0 = add(self.reg1, 1);
            return;
        }
        if self.reg1 == 0 {
            // self.reg0 = add(self.reg0, 32767);
            self.reg0 -= 1;
            self.reg1 = self.reg7;
            self.f();
            return;
        }
        self.stack.push(self.reg0);
        // self.reg1 = add(self.reg1, 32767);
        self.reg1 -= 1;
        self.f();
        self.reg1 = self.reg0;
        self.reg0 = self.stack.pop().unwrap();
        // self.reg0 = add(self.reg0, 32767);
        self.reg0 -= 1;
        self.f();
        return;
    }

    // First iteration: We remove the stack and use an intermediate value on
    // the actual call stack.
    //
    // (We're able to remove the stack because every push corresponds to a pop
    // in the same call frame)
    //
    // fn f(&mut self) {
    //     // When reg0 hits zero, restart it from reg1 + 1
    //     if self.reg0 == 0 {
    //         self.reg0 = add(self.reg1, 1);
    //         return;
    //     }

    //     // When reg1 hits zero, decrement reg0, restart reg1 from reg7
    //     if self.reg1 == 0 {
    //         self.reg0 -= 1;
    //         self.reg1 = self.reg7;
    //         self.f();
    //         return;
    //     }

    //     let save_reg0 = self.reg0;

    //     self.reg1 -= 1;
    //     self.f();
    //     self.reg1 = self.reg0;

    //     self.reg0 = save_reg0;
    //     self.reg0 -= 1;

    //     self.f();
    //     return;
    // }
}

// Second iteration: Can I remove the shared state altogether?
// fn f(mut reg0 : u16, mut reg1 : u16, mut reg7 : u16) -> (u16, u16) {
//     if reg0 == 0 {
//         reg0 = add(reg1, 1);
//         return (reg0, reg1);
//     }
//
//     if reg1 == 0 {
//         reg0 -= 1;
//         reg1 = reg7;
//         return f(reg0, reg1, reg7);
//     }
//
//     let save_reg0 = reg0;
//     reg1 -= 1;
//
//     let (reg0_, reg1_) = f(reg0, reg1, reg7);
//     reg0 = reg0_;
//     reg1 = reg1_;
//
//     reg1 = reg0;
//
//     reg0 = save_reg0;
//     reg0 -= 1;
//
//     return f(reg0, reg1, reg7);
// }

// Some simplification
fn f(reg0 : u16, reg1 : u16, reg7 : u16) -> (u16, u16) {
    if reg0 == 0 {
        return (add(reg1, 1), reg1);
    }

    if reg1 == 0 {
        return f(reg0 - 1, reg7, reg7);
    }

    let (reg1, _) = f(reg0, reg1 - 1, reg7);

    return f(reg0 - 1, reg1, reg7);
}

// OK.. now this is a function we can easily memoize.
fn f_memo(reg0 : u16, reg1 : u16, reg7 : u16, memo : &mut HashMap<(u16, u16), (u16, u16)>) -> (u16, u16) {
    if let Some(ret) = memo.get(&(reg0, reg1)) {
        return *ret;
    }

    if reg0 == 0 {
        let ret = (add(reg1, 1), reg1);
        memo.insert((reg0, reg1), ret);
        return ret;
    }

    if reg1 == 0 {
        let ret = f_memo(reg0 - 1, reg7, reg7, memo);
        memo.insert((reg0, reg1), ret);
        return ret;
    }

    // careful there
    let (reg1_new, _) = f_memo(reg0, reg1 - 1, reg7, memo);

    let ret = f_memo(reg0 - 1, reg1_new, reg7, memo);
    memo.insert((reg0, reg1), ret);
    return ret;
}

// Now that's super fast when compared to older versions...
fn search(start_range : u16, end_range : u16) -> Option<u16> {
    for i in start_range .. end_range {
        // println!("i = {}", i);
        let mut tbl = HashMap::new();
        let ret = f_memo(4, 1, i, &mut tbl);
        if ret.0 == 6 {
            println!("Found an answer: {:?} {}", ret, i);
            return Some(i);
        }
    }

    None
}

fn search_in_parallel(n : i32) {
    let increment = u16::MAX / (n as u16);
    let mut threads = Vec::with_capacity(n as usize);

    for i in 0 .. n {
        let range_start = increment * (i as u16);
        let range_end   = increment * ((i as u16) + 1) + 1;
        threads.push(thread::Builder::new().stack_size(1000000000).spawn(move || {
            search(range_start, range_end)
        }).unwrap());
    }

    for thread in threads {
        thread.join();
    }
}

fn main() {
    println!("running");
    // let mut fn_state = FnState::init(4, 1, 1);
    // fn_state.f();
    // println!("{:?}", fn_state);
    // println!("{:?}", f(4, 1, 1));
    // let mut memo = HashMap::new();
    // println!("{:?}", f_memo(4, 1, 1, &mut memo));

    // Do the search using fast version
    // println!("{:?}", search(0, u16::MAX));

    // Actually, we can even search in parallel. Split numbers in N and give
    // each one to a thread.
    search_in_parallel(8);

    // You should see something like:
    //
    //   running
    //   Found an answer: (6, 5) 25734
    //
    // At that point just stop the process and use the number!
}
