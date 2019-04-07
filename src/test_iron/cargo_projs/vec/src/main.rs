use std::env;

fn main() {
    let v = vec![1, 2, 3];
    let one_arg = if env::args().len() == 1 {
        true
    } else {
        false
    };
    println!("Hello, \"hello {:?} {:?} {:?} {:?}\"",
             env::args().len(), v[0], v[env::args().len()], !one_arg);
}
