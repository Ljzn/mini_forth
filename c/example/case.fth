: num2word 
  1 over = if drop "one" else
  2 over = if drop "two" else
  3 over = if drop "three" endif endif endif ;

: boxtest 6 > rot 22 > rot 19 > rot & & if "big enough" endif ;

: test
    2 num2word "two" =!
    23 20 7 boxtest "big enough" =! ;