// WARNING: the features after applying `to_llvm_feature` must be known
// to LLVM or the feature detection code will walk past the end of the feature
// array, leading to crashes.

pub const ARM_WHITELIST: &[(&str, Option<&str>)] = &[
    ("aclass", Some("arm_target_feature")),
    ("mclass", Some("arm_target_feature")),
    ("rclass", Some("arm_target_feature")),
    ("dsp", Some("arm_target_feature")),
    ("neon", Some("arm_target_feature")),
    ("v5te", Some("arm_target_feature")),
    ("v6k", Some("arm_target_feature")),
    ("v6t2", Some("arm_target_feature")),
    ("v7", Some("arm_target_feature")),
    ("vfp2", Some("arm_target_feature")),
    ("vfp3", Some("arm_target_feature")),
    ("vfp4", Some("arm_target_feature")),
];

pub const AARCH64_WHITELIST: &[(&str, Option<&str>)] = &[
    ("fp", Some("aarch64_target_feature")),
    ("neon", Some("aarch64_target_feature")),
    ("sve", Some("aarch64_target_feature")),
    ("crc", Some("aarch64_target_feature")),
    ("crypto", Some("aarch64_target_feature")),
    ("ras", Some("aarch64_target_feature")),
    ("lse", Some("aarch64_target_feature")),
    ("rdm", Some("aarch64_target_feature")),
    ("fp16", Some("aarch64_target_feature")),
    ("rcpc", Some("aarch64_target_feature")),
    ("dotprod", Some("aarch64_target_feature")),
    ("v8.1a", Some("aarch64_target_feature")),
    ("v8.2a", Some("aarch64_target_feature")),
    ("v8.3a", Some("aarch64_target_feature")),
];

pub const X86_WHITELIST: &[(&str, Option<&str>)] = &[
    ("adx", Some("adx_target_feature")),
    ("aes", None),
    ("avx", None),
    ("avx2", None),
    ("avx512bw", Some("avx512_target_feature")),
    ("avx512cd", Some("avx512_target_feature")),
    ("avx512dq", Some("avx512_target_feature")),
    ("avx512er", Some("avx512_target_feature")),
    ("avx512f", Some("avx512_target_feature")),
    ("avx512ifma", Some("avx512_target_feature")),
    ("avx512pf", Some("avx512_target_feature")),
    ("avx512vbmi", Some("avx512_target_feature")),
    ("avx512vl", Some("avx512_target_feature")),
    ("avx512vpopcntdq", Some("avx512_target_feature")),
    ("bmi1", None),
    ("bmi2", None),
    ("cmpxchg16b", Some("cmpxchg16b_target_feature")),
    ("fma", None),
    ("fxsr", None),
    ("lzcnt", None),
    ("mmx", Some("mmx_target_feature")),
    ("pclmulqdq", None),
    ("popcnt", None),
    ("rdrand", None),
    ("rdseed", None),
    ("sha", None),
    ("sse", None),
    ("sse2", None),
    ("sse3", None),
    ("sse4.1", None),
    ("sse4.2", None),
    ("sse4a", Some("sse4a_target_feature")),
    ("ssse3", None),
    ("tbm", Some("tbm_target_feature")),
    ("xsave", None),
    ("xsavec", None),
    ("xsaveopt", None),
    ("xsaves", None),
];

pub const HEXAGON_WHITELIST: &[(&str, Option<&str>)] = &[
    ("hvx", Some("hexagon_target_feature")),
    ("hvx-double", Some("hexagon_target_feature")),
];

pub const POWERPC_WHITELIST: &[(&str, Option<&str>)] = &[
    ("altivec", Some("powerpc_target_feature")),
    ("power8-altivec", Some("powerpc_target_feature")),
    ("power9-altivec", Some("powerpc_target_feature")),
    ("power8-vector", Some("powerpc_target_feature")),
    ("power9-vector", Some("powerpc_target_feature")),
    ("vsx", Some("powerpc_target_feature")),
];

pub const MIPS_WHITELIST: &[(&str, Option<&str>)] = &[
    ("fp64", Some("mips_target_feature")),
    ("msa", Some("mips_target_feature")),
];

pub const WASM_WHITELIST: &[(&str, Option<&str>)] = &[
    ("simd128", Some("wasm_target_feature")),
    ("atomics", Some("wasm_target_feature")),
];

/// When rustdoc is running, provide a list of all known features so that all their respective
/// primtives may be documented.
///
/// IMPORTANT: If you're adding another whitelist to the above lists, make sure to add it to this
/// iterator!
pub fn all_known_features() -> impl Iterator<Item=(&'static str, Option<&'static str>)> {
    ARM_WHITELIST.iter().cloned()
        .chain(AARCH64_WHITELIST.iter().cloned())
        .chain(X86_WHITELIST.iter().cloned())
        .chain(HEXAGON_WHITELIST.iter().cloned())
        .chain(POWERPC_WHITELIST.iter().cloned())
        .chain(MIPS_WHITELIST.iter().cloned())
        .chain(WASM_WHITELIST.iter().cloned())
}
