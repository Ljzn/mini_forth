: not 0 num= if 1 else 0 endif ;

: rot tas swap fas swap ;

: 2dup over over ;

: 3dup 2 pick 2 pick 2 pick ;

: 2swap rot tas rot fas ;

: 2rot tas tas 2swap fas fas 2swap ;

: ifdup dup if dup endif ;

: 2over tas tas 2dup fas fas 2swap ;

: tuck swap over ;

: 2drop drop drop ;

: nip swap drop ;

: 1add 1 + ;

: 1sub 1 - ;

: min 2dup < if drop else nip endif ;

: max 2dup > if drop else nip endif ;

: not0 not not ;

: or + not0 ;

: < - dup if size 1- dup if 0 swap num2bin endif <<0x80>> cat & 1 rshift not0 else drop 0 endif ;

: > 2dup < tas num= fas or not ;

: >= < not ;

: <= > not ;

: hash256 sha256 sha256 ;

: hash160 sha256 ripemd160 ;

: within 2 pick > tas >= fas and ;

: and * 0 num= not ;

: verify not 0 num=verify ;