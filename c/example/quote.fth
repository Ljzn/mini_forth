: keep
    over [ call ] dip ; inline

: bi
    [ keep ] dip call ; inline

: main 4 3 1 [ + ] [ + ] bi ;