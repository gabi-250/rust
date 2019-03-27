use std::env;

pub enum Align64 {
    A(u32),
    B(u16),
    C(u8),
    D(i32, i32),
}

pub fn align64(a: u32) -> Align64 {
    let a64 = if a == 1 {
        Align64::C(a as u8 * 3)
    } else if a == 2 {
        Align64::D(a as i32, a as i32 * 2)
    } else if a == 3 {
        Align64::A(a + 2)
    } else {
        Align64::B(a as u16)
    };
    a64
}

fn main() {
    let argc = env::args_os();
    let code = {
        let x = align64(argc.len() as u32);
        match x {
            Align64::A(y) => y,
            Align64::B(y) => y as u32 + 1,
            Align64::C(y) => y as u32 + 2,
            Align64::D(y, z) => y as u32 + z as u32,
        }
    };

    std::process::exit(code as i32);
}

