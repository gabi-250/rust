// run-rustfix
// edition:2018
// compile-pass
// aux-build:remove-extern-crate.rs
// compile-flags:--extern remove_extern_crate

#![feature(alloc)]
#![warn(rust_2018_idioms)]

extern crate core;
extern crate core as another_name;
use remove_extern_crate;
#[macro_use]
extern crate remove_extern_crate as something_else;

// Shouldn't suggest changing to `use`, as the `alloc`
// crate is not in the extern prelude - see #54381.
extern crate alloc;

fn main() {
    another_name::mem::drop(3);
    another::foo();
    remove_extern_crate::foo!();
    bar!();
    alloc::vec![5];
}

mod another {
    extern crate core;
    use remove_extern_crate;

    pub fn foo() {
        core::mem::drop(4);
        remove_extern_crate::foo!();
    }
}
