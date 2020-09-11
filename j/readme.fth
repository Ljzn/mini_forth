: power ( a b -- a^b )
    1 swap max 0 do
        dup if
            tas over * fas 1sub
        endif
    loop drop nip
;

\ max iterations
: max 10 ;

: main 4 3 power ;