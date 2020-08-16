\ change the endian of bytes

: reverse_bytes [ bytes size ] literal 0 do 1 split loop [ bytes size ] literal 0 do swap cat loop ;

: bytes "abcdefg" ;

: main bytes reverse_bytes ;