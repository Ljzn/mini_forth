: left split drop ;

: right split nip ;

: substr
    rot rot right
    swap left ;

: main "I am a fish" 2 2 substr ;

: test main "am" =! ;