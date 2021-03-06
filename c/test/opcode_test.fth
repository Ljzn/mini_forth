: main 

    \ test numbers
    1 <<0x01>> =verify
    8388608 <<0x00008000::4*8>> =verify
    9223372036854775807 <<0xFFFFFFFFFFFFFF7F::8*8>> =verify

    \ test negative numbers
    -1 <<0x81>> =verify
    -8388608 <<0x00008080::4*8>> =verify
    -9223372036854775807 <<0xFFFFFFFFFFFFFFFF::8*8>> =verify

    \ test num equal
    <<0x100000::3*8>> <<0x10000000::4*8>> num=verify
    <<0x000080::3*8>> <<0x00000080::4*8>> num=verify

    \ test less than
    -1 0 < verify

    \ test greater than
    1 0 > verify

    \ test less than or equal
    0 0 <= verify
    10 11 <= verify
    -11 -10 <= verify

    \ test greater than or equal
    0 0 >= verify
    11 0 >= verify
    4 4 >= verify

    \ test invert
    <<0x01, 0x00>> ~
    <<0xfe, 0xff>> =verify

    -1 ~
    <<0x7e>> =verify

    \ test xor
    0 0 ^
    0 =verify

    <<0xab>> <<0xcd>> ^
    <<0x66>> =verify

    \ test mul
    <<0x05>> <<0x06>> *
    <<0x1E>> =verify
    
    <<0xA0, 0xA0>> <<0xF5, 0xE4>> *
    <<0x20, 0xB9, 0xDD, 0x0C>> =verify

    <<0x06, 0x26, 0x09, 0x34>> <<0x26, 0x03, 0x32, 0x04>> *
    <<0xE4, 0xB6, 0xF9, 0x59, 0x05, 0x4F, 0xDA, 0x00>> =verify

    \ test add
    -2 1+
    -1 =verify

    2147483647 dup +
    4294967294 =verify

    \ test sub
    111 1-
    110 =verify

    \ test negate
    -1 negate
    1 =verify

    \ test mod
    1 1 %
    0 =verify

    -1 1 %
    0 =verify

    82 23 %
    13 =verify

    8 -3 %
    2 =verify
    
    \ test lshift
    <<0x9F, 0x11, 0xF5, 0x55>> 
    
    dup <<0x01>> lshift
    <<0b00111110001000111110101010101010::32>> =verify

    dup <<0x02>> lshift
    <<0b01111100010001111101010101010100::32>> =verify

    dup <<0x0F>> lshift
    <<0b11111010101010101000000000000000::32>> =verify

    \ test rshift
    dup <<0x01>> rshift
    <<0b01001111100010001111101010101010::32>> =verify

    dup <<0x02>> rshift
    <<0b00100111110001000111110101010101::32>> =verify

    dup <<0x0F>> rshift
    <<0b00000000000000010011111000100011::32>> =verify

    \ test if
    0 if 0 else 1 endif verify
    1 if 1 else 0 endif verify

    \ test nofif
    0 notif 1 else 0 endif verify
    1 notif 0 else 1 endif verify

    \ test roll
    22 21 20 0 roll
    20 =verify

    22 21 20 2 roll
    22 =verify

    \ test pick
    22 21 20 0 pick
    20 =verify

    2 pick
    22 =verify

    \ test drop
    0 1 2 3 drop
    2 =verify
    drop
    0 =verify

    \ test swap
    1 2 swap
    1 =verify
    2 =verify

    \ test over
    1 2 over
    1 =verify
    2 =verify
    1 =verify

    \ test 2over
    1 2 3 4 2over
    2 =verify
    1 =verify
    4 =verify
    3 =verify
    2 =verify
    1 =verify

    \ test tuck
    1 2 tuck
    2 =verify
    1 =verify
    2 =verify

    \ test dup
    1 dup
    1 =verify
    1 =verify

    \ test 2dup
    1 2 2dup
    2 =verify
    1 =verify
    2 =verify
    1 =verify

    \ test 3dup
    1 2 3 3dup
    3 =verify
    2 =verify
    1 =verify
    3 =verify
    2 =verify
    1 =verify

;