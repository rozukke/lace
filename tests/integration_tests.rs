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
fn runs_stack_example() {
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("run")
        .arg("tests/files/stack.asm")
        .arg("--features")
        .arg("stack");

    cmd.assert()
        .success()
        .stdout(contains("Hello from the stack"))
        .stdout(contains("Returned from call"))
        .stdout(contains("R2 contents are 5"));
}

#[test]
fn runs_recursive_fibonacci_example() {
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("run")
        .arg("tests/files/fibonacci.asm")
        .arg("--features")
        .arg("stack");

    // Hardcoded 23rd fibonacci number
    cmd.assert().success().stdout(contains("28657"));
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
