: mycat ( a b -- ab )
    2dup padding_left_with_size_of_a tas
    padding_right_to_size_of_ab
    fas
    |
;

: padding_left_with_size_of_a ( a b -- __b )
    over padding_right_to_size_of_ab
    swap size! bits rshift
;

: size! size nip ;

: bits 8 * ;

: padding_right_to_size_of_ab ( a b -- a__ )
    over sum_size num2bin
;

: sum_size ( a b -- s )
    size! swap size! +
;

: main "abc" "de" mycat "abcde" =verify ;