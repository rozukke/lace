main:
        ld r0 n
        call fib
        reg
        halt

; variables
n:      .fill #23

; n in r0, result in r1
fib:
        push r2
        ; accumulator
        and r1 r1 #0
        ; workspace
        and r2 r2 #0

        push r0
        call fib_inner
        pop r0

        pop r2
        rets

fib_inner:
        ; access stack variable
        ldr r0 r7 #1
        and r2 r2 #0
        add r2 r0 #-1
        brnz fib_post

        add r0 r0 #-1
        push r0
        call fib_inner
        pop r0

        add r0 r0 #-1
        push r0
        call fib_inner
        pop r0

        rets

; bottom of recursive call
fib_post:
        add r1 r1 r0
        rets
