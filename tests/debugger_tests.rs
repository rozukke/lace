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
        .arg(
            "
            echo lea; reg
            progress
            echo puts; reg
            progress
            echo halt; reg
            continue
        ",
        );

    cmd.assert()
        .success()
        .stdout(contains("Hello, world!"))
        .stdout(contains("Halted"))
        .stderr(diff(
            // Empty line is where "Hello, world!" is printed to stdout
            "\
                [lea]\n\
                R0 0x0000\n\
                R1 0x0000\n\
                R2 0x0000\n\
                R3 0x0000\n\
                R4 0x0000\n\
                R5 0x0000\n\
                R6 0x0000\n\
                R7 0xfdff\n\
                PC 0x3000\n\
                CC 0b000\n\
                [puts]\n\
                R0 0x3003\n\
                R1 0x0000\n\
                R2 0x0000\n\
                R3 0x0000\n\
                R4 0x0000\n\
                R5 0x0000\n\
                R6 0x0000\n\
                R7 0xfdff\n\
                PC 0x3001\n\
                CC 0b001\n\
                \n\
                Reached HALT. Pausing execution.\n\
                [halt]\n\
                R0 0x3003\n\
                R1 0x0000\n\
                R2 0x0000\n\
                R3 0x0000\n\
                R4 0x0000\n\
                R5 0x0000\n\
                R6 0x0000\n\
                R7 0xfdff\n\
                PC 0x3002\n\
                CC 0b001\n\
            ",
        ));
}
