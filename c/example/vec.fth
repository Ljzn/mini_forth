: vec "" ; ( -- bytes )

: push 1 num2bin cat ; ( bytes x -- bytesx )

: at swap dup rot split nip 1 split drop ; ( bytes index -- bytes x )

: main vec 66 push 105 push 116 push 99 push 111 push 105 push 110 push 3 at ; ( -- bytes x)