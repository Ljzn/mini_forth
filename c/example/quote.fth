: keep
    over [ call ] dip ; inline

: bi
    [ keep ] dip call ; inline

: main 1 [ 2 + ] [ 3 + ] bi ;