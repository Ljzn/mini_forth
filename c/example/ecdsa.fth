\ ECDSA signing

\ args. these values can be replaced
: privkey [ rand_number ] literal ;
: msg "hello" ;

\ G
: gx 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 ;
: gy 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8 ;

: pubkey ( privkey_int -- pubkey_bytes )
    privkey_to_compressed_pubkey ;

: n 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141 ;
\ Prime field order
: p 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F ;

: r ( -- r_int )
    gx ;

: s ( msg privkey_int -- s_int )
    r * swap sha256 be_32bytes_to_integer + n % ;

: pf_mul ( x y -- z )
    * p % ;

: k ( -- k )
    [ rand_number ] literal ;

\ helpers
: be_32bytes_to_integer reverse_bytes <<0>> cat bin2num ;
: integer_to_32be_bytes 33 num2bin 32 split drop ;
( BE_Hash -- LE_Hash )
: reverse_bytes 32 0 do 1 split loop 32 0 do swap cat loop ;
( -- integer )
: rand_number 32 rand_bytes <<0>> cat bin2num ;

: ecdsa_verify ( pubkey signature hash -- bool )
    ecdsa_verify ;

: dersig_encode ( r s -- signature )
    dersig_encode ;

\ tests
: test
    k dup pf_inv pf_mul
    1
    =verify

    0x94425846858831972197980231481249834738537631726645285884532617334591823456590 pubkey
    <<3, 129, 96, 10, 247, 156, 9, 146, 225, 89, 40, 115, 172, 157, 162, 115, 218, 238, 77, 185, 216, 101, 228, 252, 1, 138, 238, 87, 81, 126, 55, 232, 240>>
    =verify

    msg privkey 2dup s r swap dersig_encode
    \ -- msg privkey signature
    swap pubkey
    \ -- msg signature pubkey
    swap 2 pick
    \ -- pubkey signature msg
    sha256 ecdsa_verify .
;