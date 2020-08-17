\ simple ECDSA signing
\ k is 1
: gx 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 ;

: n 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141 ;

: r gx ;

( msg s1 -- s )
( s1 is the privatekey in integer)
: s r * swap sha256 reverse_bytes <<0>> cat bin2num + n % ;

( BE_Hash -- LE_Hash )
: reverse_bytes 32 0 do 1 split loop 32 0 do swap cat loop ;

\ : main "hello" 0x8234DA68A1ACC82378667E5ED4A15C051FF96D7630761323E92C2EB493B95A2C s ;

( Prime field order )
: p 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F ;

: rand_number 32 rand_bytes <<0>> cat bin2num ;

( x y -- z )
: pf_mul * p % ;

: k [ rand_number ] literal ;

( k scalar mul the inversion of k always equal to 1 )
: test k dup pf_inv pf_mul 1 =! ;