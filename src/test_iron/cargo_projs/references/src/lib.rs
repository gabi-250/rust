#[no_mangle]
pub extern "C" fn inc(y: &mut u16) -> u16 {
    *y = *y + 1;
    *y
}

#[no_mangle]
pub extern "C" fn references(x: u16) -> u16 {
    let mut y = x;
    let z = &mut y;
    *z = *z + 1;
    *z
}

#[no_mangle]
pub extern "C" fn references2<'a>(x: &'a mut u16) -> &'a u16 {
    let z: &mut u16 = x;
    *z = *z + 1;
    z
}

#[no_mangle]
pub extern "C" fn references3<'a>(x: &'a mut u16, y: u16) -> &'a u16 {
    let z: &mut u16 = x;
    *z = *z + y;
    z
}
