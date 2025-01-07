    and r0, r0, #0

Loop
    REG

    add r0, r0, #1
    ld r1, NegMax
    add r1, r0, r1
    BRn Loop

    HALT

NegMax .FILL x-81

