use std::fmt::Write;

use build_rs::input::{
    cargo_cfg_target_abi, cargo_cfg_target_arch, cargo_cfg_target_endian, cargo_cfg_target_env,
    cargo_cfg_target_os, cargo_cfg_target_pointer_width, cargo_cfg_target_vendor,
};

fn main() {
    let mut buf = String::new();
    buf.push_str("const TARGET: BuildTarget = BuildTarget {\n");
    writeln!(&mut buf, "  arch: {:?},", cargo_cfg_target_arch()).unwrap();
    writeln!(&mut buf, "  endian: {:?},", cargo_cfg_target_endian()).unwrap();
    writeln!(&mut buf, "  os: {:?},", cargo_cfg_target_os()).unwrap();
    writeln!(
        &mut buf,
        "  family: &{:?},",
        &std::env::var("CARGO_CFG_TARGET_FAMILY")
            .unwrap()
            .split(',')
            .collect::<Vec<_>>()
    )
    .unwrap();
    writeln!(&mut buf, "  env: {:?},", cargo_cfg_target_env()).unwrap();
    writeln!(&mut buf, "  abi: {:?},", cargo_cfg_target_abi()).unwrap();
    writeln!(
        &mut buf,
        "  pointer_width: {},",
        cargo_cfg_target_pointer_width()
    )
    .unwrap();
    writeln!(&mut buf, "  vendor: {:?}", cargo_cfg_target_vendor()).unwrap();
    buf.push_str("};");
    std::fs::write(build_rs::input::out_dir().join("target.rs"), &buf).unwrap();
}
