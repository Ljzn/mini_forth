: keep
    over [ call ] dip ; inline

: bi
    [ keep ] dip call ; inline

: tri
    [ [ keep ] dip keep ] dip call ; inline

: main 1 [ 1 + ] [ 2 + ] [ 3 + ] tri ;