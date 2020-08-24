\ token 

: main ( txpreimage -- )
    hash256 reverse_32bytes <<0>> cat
    \ -- hash_in_integer
    r*private_key + inverse_k * n tuck %
    \ -- n s
    2dup
    \ -- n s n s
    swap 2 /
    \ -- n s s n/2
    > if - else nip endif
    \ -- low_s
    size dup 37 +
    \ -- s len_of_s len_of_s+37
    <<48>> swap cat
    \ -- s len_of_s sig..
    r_for_der cat
    \ -- s len_of_s sig..
    swap cat
    \ -- s sig..
    swap
    \ -- sig.. s
    reverse_n_bytes
    


    
;

: reverse_32bytes ( bytes -- bytes1 )
    32 0 do 1 split loop 32 0 do swap cat loop ;

: s ();