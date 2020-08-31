\ Consts

: R_MUL_PRIVATEKEY 0 ;
: INVERSE_K 0 ;
: SIGHASH_FLAG 0 ;
: PUBKEY 0 ;
: R_FOR_DER 0 ;

\ OP_PUSH_TX

: push_tx ( preimage -- bool )
    hash256 hash2num
    R_MUL_PRIVATEKEY +
    INVERSE_K *
    N tuck %
    low_s
    der_encoding
    SIGHASH_FLAG cat
    PUBKEY checksigverify
;

: hash2num ( bytes -- int )
    32 reverse_n_bytes <<0>> cat
;

: COMPOND <<0x30>> ;

: der_encoding ( s -- sig )
    size dup 37 +
    COMPOND swap cat
    R_FOR_DER cat
    swap cat swap
    32 reverse_n_bytes
;

: N 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141 ;

\ Reverse the endian of a bytes with unknown length.
\ Should has a constant of max possible length.

: reverse_n_bytes ( bytes max_len -- reversed_bytes )
    [ split_and_swap_n_bytes ] [ concat_n_bytes ] bi ;

\ helpers

: split_and_swap_n_bytes ( bytes max_len -- byte... )
    0 do size dup if 1- split swap endif loop ;

: concat_n_bytes ( byte... max_len -- bytes )
    0 do cat loop ;


\ Checking s, if s is high, change it to low s.

: low_s ( n s -- s1 )
    2dup swap 2 div > if - 
    else nip endif ;

: main <<"abcdefg">> 32 reverse_n_bytes ;