#[no_mangle]
pub extern "C" fn fibonacci(x: u64) -> u64 {
    if x < 2 {
        return x
    } else {
        fibonacci(x - 1) + fibonacci(x - 2)
    }
}
