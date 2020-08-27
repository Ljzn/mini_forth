\ This file is modified from https://github.com/factor/factor/
\
\ Copyright (C) 2006, 2010 Slava Pestov, Daniel Ehrenberg.
\ See http://factorcode.org/license.txt for BSD license.

: keep
    over [ call ] dip ; inline

: bi
    [ keep ] dip call ; inline

: tri
    [ [ keep ] dip keep ] dip call ; inline

: 2dip ( x y quot -- x y ) swap [ dip ] dip ; inline

: tri* ( x y z p q r -- )
    [ [ 2dip ] dip dip ] dip call ; inline

: tri@ ( x y z quot -- )
    dup dup tri* ; inline