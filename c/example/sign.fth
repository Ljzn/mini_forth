( do sign on-chain )
( the things we already know: )
( - message: m )
( - private key: s1 )
( - signature: r and s )

( message )
: m "hello" ;

( privatekey in integer )
: s1 0x9060D325C176786A222D63E77D209269009C07C1A588FBEDD8C744204016C834 ;

: gx 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 ;

: n 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141 ;

: r gx ;

: s m sha256 s1 r * + n % ;

( r r_size s s_size -- encoded_signature)
: der_sig_encode 42 ;

: main r s der_sig_encode ;