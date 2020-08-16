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

( msg s1 -- s )
: main "hello" 0x8234DA68A1ACC82378667E5ED4A15C051FF96D7630761323E92C2EB493B95A2C s ;