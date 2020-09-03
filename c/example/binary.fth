\ sub string

: left ( str n -- left_str )
    split drop ;

: right ( str n -- right_str )
    split nip ;

: substr ( str start len -- substr )
    rot rot right
    swap left ;

: main "I am a fish" 2 4 substr ;

: test main "am a" =verify ;