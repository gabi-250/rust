#[no_mangle]
pub extern "C" fn call_exit(x: i32) {
    std::process::exit(x);
}
