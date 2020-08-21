: keep
    over [ call ] dip ; inline

: bi
    [ keep ] dip call ; inline

: tri
    [ [ keep ] dip keep ] dip call ; inline

: 2dip ( x y quot -- x y ) swap [ dip ] dip ; inline