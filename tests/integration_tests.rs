use assert_cmd::prelude::*;
use std::process::Command;

#[test]
fn runs_without_arguments() {
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.assert().success();
}
