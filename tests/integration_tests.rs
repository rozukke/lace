use assert_cmd::Command;
use predicates::str::contains;
use tempfile::tempdir;

#[test]
fn runs_without_arguments() {
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.assert().success();
}

#[test]
fn runs_hello_world() {
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("run").arg("tests/files/hw.asm");

    cmd.assert()
        .success()
        .stdout(contains("Hello, world!"))
        .stdout(contains("Halted"));
}

#[test]
fn compile_and_run() {
    let dir = tempdir().expect("Could not make tempdir");

    let outfile_path = dir.path().join("hw.lc3");

    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("compile")
        .arg("tests/files/hw.asm")
        .arg(&outfile_path);

    cmd.assert().success().stdout(contains("Saved target"));

    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg(&outfile_path);

    cmd.assert().success().stdout(contains("Hello, world!"));
}
