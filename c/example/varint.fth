\ read veriable length interger from a bytes
\ return the integer and remain bytes

: read_varint ( bytes -- int bytes1 )
    1 split tas
    dup = <<0xFD>> if drop fas 2 split else
    dup = <<0xFE>> if drop fas 4 split else
    dup = <<0xFF>> if drop fas 8 split else
    fas cat 0 endif endif endif
    swap add_sign swap
;

\ helpers

: add_sign ( bytes -- int )
    <<0>> cat
;

\ integer to varint

: varint ( int -- bytes )
    no_negative
    bin2num
    dup notif <<0>> else
    choose_prefix_and_size
    rot swap to_unsign
    endif
    cat
;

\ helpers

: choose_prefix_and_size ( int -- int prefix size ) 
    dup 0xfd < if 0 1 else
    dup 0xffff <= if <<0xFD>> 2 else
    dup 0xffffffff <= if <<0xFE>> 4 else
    dup 0xffffffffffffffff <= if <<0xFF>> 8 else
    \ abort when number too large
    0 verify 
    endif endif endif endif
;

: no_negative ( int -- )
    dup 0 >= verify
;

: to_unsign ( int size -- bytes )
    dup tas 1+ num2bin fas split drop
;

: last_byte ( a -- byte )
    size 1- split nip
;

: main
    \ read_varint
    <<0xFD, 0x00, 0x01, 0xFF>> read_varint
    <<0xFF>> =verify
    256 num=verify

    \ varint
    65535 varint
    <<0xFD, 0xFF, 0xFF>> =verify

    256 varint
    <<0xFD, 0x00, 0x01>> =verify

    0 varint
    <<0>> =verify
;