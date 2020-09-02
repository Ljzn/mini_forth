\ read veriable length interger from a bytes
\ return the integer and remain bytes

: read_varint ( bytes -- int bytes1 )
    1 split tas
    dup = <<0xFD>> if drop fas 2 split else
    dup = <<0xFE>> if drop fas 4 split else
    dup = <<0xFF>> if drop fas 8 split else
    fas cat 0 endif endif endif
    swap add_sign print_stack swap
;

\ helpers

: add_sign ( bytes -- int )
    <<0>> cat
;

: main
    <<0xFD, 0x00, 0x01, 0xFF>> read_varint
    <<0xFF>> =verify
    256 num=verify    
;