use assert_cmd::Command;
use predicates::str::{contains, diff};

#[test]
fn runs_hello_world() {
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("debug").arg("tests/files/hw.asm");

    cmd.assert()
        .success()
        .stdout(contains("Hello, world!"))
        .stdout(contains("Halted"));
}

#[test]
fn debugs_hello_world() {
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("debug")
        .arg("tests/files/hw.asm")
        .arg("--minimal")
        .arg("--command")
        .arg(include_str!("commands/hello_world"));

    cmd.assert()
        .success()
        .stdout(contains("Hello, world!"))
        .stdout(contains("Halted"))
        .stderr(diff(
            include_str!("expected/hello_world").replace("\r\n", "\n"),
        ));
}

#[test]
fn prints_help_message() {
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("debug").arg("--print-help").arg("--minimal");
    cmd.assert().success().stderr(diff(minimal_help_message()));

    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("debug")
        .arg("tests/files/hw.asm")
        .arg("--minimal")
        .arg("--command")
        .arg("help");
    cmd.assert()
        .success()
        .stderr(diff(minimal_help_message() + "\n"));
}

fn minimal_help_message() -> String {
    // Remove all `{...}`
    let mut expected = String::new();
    expected.push('\n');
    let mut expected_raw = include_str!("../src/debugger/help.txt").chars();
    while let Some(ch) = expected_raw.next() {
        if ch != '{' {
            expected.push(ch);
            continue;
        }
        for ch in expected_raw.by_ref() {
            if ch == '}' {
                break;
            }
        }
    }
    expected.push('\n');
    expected
}

#[test]
fn check_every_command() {
    // Except `quit`, `help`
    let mut cmd = Command::cargo_bin("lace").unwrap();
    cmd.arg("debug")
        .arg("tests/files/hw.asm")
        .arg("--minimal")
        .arg("--command")
        .arg(include_str!("commands/check_every_command"));

    cmd.assert()
        .success()
        .stdout(contains("Hello, world!"))
        .stderr(diff(
            include_str!("expected/check_every_command").replace("\r\n", "\n"),
        ));
}
