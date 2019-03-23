#[no_mangle]
pub extern "C" fn sum_to_n(n: u16) -> u16 {
    if n == 0 {
        return 0;
    } else {
        n + sum_to_n(n - 1)
    }
}

#[no_mangle]
pub extern "C" fn iter_sum_to_n(n: u16) -> u16 {
    let mut i = 0;
    let mut sum = 0;
    while i < n {
        i += 1;
        sum += i;
    }
    sum
}
