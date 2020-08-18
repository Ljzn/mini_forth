: 2dup over over ;
: 3dup 2 pick 2 pick 2 pick ;
: 2swap rot tas rot fas ;
: 2rot tas tas 2swap fas fas 2swap ;
: ifdup dup if dup endif ;
: 2over tas tas 2dup fas fas 2swap ;
: tuck dup rot swap ;