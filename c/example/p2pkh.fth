: p2pkh dup hash_puzzle sigverify ;

: hash_puzzle hash160 "expected hash of pubkey" =! ;

: main "signature" "pubkey" p2pkh ;