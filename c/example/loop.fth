: factorial 
    1 + 1 tas 10 1 do
        dup i > if
            fas i * tas
        endif
    loop drop fas ;

: main 4 factorial ;

: test 4 factorial 24 =! ;