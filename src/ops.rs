use crate::state::Flag;
use crate::symbol::Register;

enum Opcodes {
    // Add SR1 (source register 1) with SR2 and store in DR (destination register)
    ADD {
        dest_r: Register,
        src_r_1: Register,
        src_r_2: Choice,
    },
    // Bitwise AND SR1 with SR2 and store in DR
    AND {
        dest_r: Register,
        src_r_1: Register,
        src_r_2: Choice,
    },
    // Branch based on flag by adding offset to PC
    BR {
        cc: Flag,
        pc_offset9: u16,
    },
    // Set PC to BaseR
    JMP {
        base_r: Register,
    },
    JSR {
        pc_offset11: u16,
    },
    JSRR {
        base_r: Register,
    },
    LD {
        dest_r: Register,
        pc_offset9: u16,
    },
    LDI {
        dest_r: Register,
        pc_offset9: u16,
    },
    LDR {
        dest_r: Register,
        base_r: Register,
        pc_offset6: u16,
    },
    LEA {
        dest_r: Register,
        pc_offset9: u16,
    },
    NOT {
        dest_r: Register,
        src_r: Register,
    },
    RET,
    RTI,
    ST {
        src_r: Register,
        pc_offset9: u16,
    },
    STI {
        src_r: Register,
        pc_offset9: u16,
    },
}

// ADD and AND commands support immediate value
enum Choice {
    Reg(Register),
    Imm5(u8),
}
