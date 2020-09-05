\ we can define such a function
\ which takes 2 args, one is compile
\ time arg, and another is runtime arg

: simple_macro ( compile_time_arg runtime_arg -- a b )
    [ 3 + ]
    macro_start
        swap tas curry eval fas
    macro_end
    +
;

\ compiled code: 4 2 + ;
: main 1 2 simple_macro ;