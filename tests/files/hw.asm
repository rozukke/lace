; comment
    jsr foo
    reg
    halt

foo:
    lea r0 hw
    puts
    ret

hw .stringz "Hello, world!"

