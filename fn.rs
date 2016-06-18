#![feature(alloc_system)]
extern crate alloc_system;

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

    fn f(&mut self) {
        // print!("f");
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
}

fn main() {
    let mut fn_state = FnState::init(4, 1, 2);
    fn_state.f();
    println!("{:?}", fn_state);
}
