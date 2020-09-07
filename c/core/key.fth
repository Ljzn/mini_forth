: import
    "core/base"
;

: compress_pubkey ( x y -- bytes )
    prefix
    swap base:to_big_unsigned256
    cat
;

\ helpers
: prefix ( y -- byte )
    2 % if
        <<3>>
    else
        <<2>>
    endif
;

: main
    1 2 compress_pubkey
    <<2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>
    =verify
;