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