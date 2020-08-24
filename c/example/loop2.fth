\ Reverse the endian of a bytes with unknown length.
\ Should has a constant of max possible length.

: reverse_n_bytes ( bytes max_len -- reversed_bytes )
    [ split_and_swap_n_bytes ] [ concat_n_bytes ] bi ;

\ helpers

: split_and_swap_n_bytes ( bytes max_len -- byte... )
    0 do size dup if 1- split swap endif loop ;

: concat_n_bytes ( byte... max_len -- bytes )
    0 do cat loop ;

: main "abcdefg" 32 reverse_n_bytes ;