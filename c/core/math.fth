: power ( a b -- a^b )
    1 swap max 0 do
        dup if
            tas over * fas 1-
        endif
    loop drop nip
;

\ helpers
: max 10 ;

: main 4 3 power ;