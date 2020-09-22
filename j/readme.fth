\ MiniForth
\
\ Lines after "\" are the comments.
\
\ Use ":" and ";" to define a word.

: add2 2 + ;

\ You can use your new word in another
\ word definition.

: add4 add2 add2 ;

\ The "(" ")" can make embeded comment,
\ we often use it as a spec.

: area_of_rect ( a b -- area )
    *
;

\ The numbers will be auto converted to
\ minimal encoded binary.

: a_large_number 999999999 ;

\ The utf8 string is also supported.

: hello "你好" ;

\ You can input arbitray size of binary
\ with "<<value:bit_size>>" syntax,
\ the value is encoded in big endian by default.

: a_binary <<1:8, 0xffabcd:64, 0xaabb:16/big, 0xaabb:16/little>> ;

\ Only the "main" word will be compiled.

: main
    a_large_number
    hello
    a_binary
    dup_3_times
;

\ You can do a simple loop with
\ "[stop] [start] do ... loop"

: dup_3_times
    3 0 do dup i loop
;

\ Embeded loop is unsupported yet.
