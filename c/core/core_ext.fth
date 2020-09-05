\ This file is modified from https://github.com/factor/factor/
\
\ Copyright (C) 2006, 2010 Slava Pestov, Daniel Ehrenberg.
\ See http://factorcode.org/license.txt for BSD license.

: dip ( a b q -- aq b )
    swap tas call fas ; macro

: keep
    over [ call ] dip ; macro

: bi
    [ keep ] dip call ; macro

: tri
    [ [ keep ] dip keep ] dip call ; macro

: 2dip ( x y quot -- x y ) swap [ dip ] dip ; macro

: tri* ( x y z p q r -- )
    [ [ 2dip ] dip dip ] dip call ; macro

: tri@ ( x y z quot -- )
    dup dup tri* ; macro