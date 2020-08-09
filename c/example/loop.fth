: factorial 
    1 + 1 tas 10 1 do
        dup i > if
            fas i * tas
        endif
    loop drop fas ;

: test 4 factorial 24 =! ;

: nested 5 1 do 5 1 do i j * loop loop ;

: main nested ;

