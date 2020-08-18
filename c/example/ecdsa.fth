\ ECDSA signing

\ args. these values can be replaced
: privkey [ rand_number ] literal ;
: msg "hello" ;

\ body
: gx 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 ;
: gy 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8 ;

( privkey@int -- pubkey )
: pubkey integer_to_32be_bytes privkey_to_compressed_pubkey ;

: n 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141 ;
( Prime field order )
: p 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F ;
( -- r )
: r gx ;
( msg privkey@int -- s )
: s r * swap sha256 be_32bytes_to_integer + n % ;
( x y -- z )
: pf_mul * p % ;
( -- k )
: k [ rand_number ] literal ;

\ helpers
: be_32bytes_to_integer reverse_bytes <<0>> cat bin2num ;
: integer_to_32be_bytes 33 num2bin 32 split drop ;
( BE_Hash -- LE_Hash )
: reverse_bytes 32 0 do 1 split loop 32 0 do swap cat loop ;
( -- integer )
: rand_number 32 rand_bytes <<0>> cat bin2num ;

\ tests
: test
    k dup pf_inv pf_mul 1 =!
    0x94425846858831972197980231481249834738537631726645285884532617334591823456590 pubkey
    <<2, 247, 242, 243, 130, 168, 214, 11, 52, 138, 158, 102, 219, 254, 185, 212, 97, 240, 216, 140, 92, 100, 180, 60, 6, 216, 92, 169, 198, 181, 208, 131, 175>> =verify
;