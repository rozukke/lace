main
    ; do subroutine with stack
    call hw_sub
    ; print return from call
    lea r0 sr
    puts
    ; test stack
    add r1 r1 #5
    push r1
    pop r2
    ; ascii offset
    ld r3 offs
    add r2 r2 r3
    ; change res string
    lea r0 res
    str r2 r0 #16
    puts
    halt

hw_sub
    lea r0 hw
    puts
    rets

hw .stringz "Hello from the stack\n"
sr .stringz "Returned from call\n"
res .stringz "R2 contents are _\n"
offs .fill 0x30
