\ base
\
\ This module provides data encoding and decoding words. 

: decode256 ( unsign_32bytes_big_endian -- integer )
    reverse256 from_uint
;

: from_uint ( unsigned -- signed )
    <<0>> cat bin2num
;

: reverse256 ( bytes -- reversed_bytes )
    32 [ 0 do 1 split loop ] [ 0 do swap cat loop ] bi
;

: main
    <<21, 57, 117, 176, 175, 132, 238, 95, 236, 249, 62, 194, 79, 101, 65, 79, 3, 96, 25, 125, 146, 247, 164, 108, 61, 55, 228, 143, 23, 235, 72, 146>>
    decode256
    9600092370698485722880671127416733821119464114058201285631723395862080735378
    =verify
;