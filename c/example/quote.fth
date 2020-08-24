: f
    [ 1 + ] [ 2 + ] bi ;

: main 1 f ;

\ : main 1 2 3 4 [ + ] 2dip ;

\ : main 1 2 3 [ 2 * ] tri@ ;

\ : main "abc" 1 split 1 split ;

\ : main "a" "b" "c" [ bin2num 32 - 1 num2bin ] tri@ cat cat ;