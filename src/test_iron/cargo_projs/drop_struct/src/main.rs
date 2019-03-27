use std::env;

struct Bob {
    name: u64,
    age: i32,
}

impl Drop for Bob {
    fn drop(&mut self) {
        println!("Dropping Bob!");
    }
}

fn main() {
    let argc = {
        let args = env::args_os();
        args.len()
    };
    let ec = {
        let mut bob = Bob { name: argc as u64 * 2, age: argc as i32 };
        bob.name = 8 * argc as u64;
        bob.name + bob.age as u64
    };
    std::process::exit(ec as i32);
}

