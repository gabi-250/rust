#![allow(dead_code)]

macro_rules! asm {
    ($m:expr, $($args:expr)*) => {
        $(
            $m.asm.push_str(&format!("{}\n", $args));
        )*
    }
}

// FIXME not yet implemented
