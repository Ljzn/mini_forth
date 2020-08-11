: P 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f ;

: A 0 ;

: B 7 ;

: G 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8 ;

: N 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141 ;

: H 1 ;

: pf_add + P % ;

: pf_sub - P + P % ;

: pf_neg P swap - ;

: pf_mul * P % ;

: pf_inv 1 swap / ;

: ec_add "infinity" = ;

: k 1 ;

