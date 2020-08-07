-module(encoding).
-export([parse_varint/1, varint/1]).

parse_varint(<<16#fd, X:16/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<16#fe, X:32/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<16#ff, X:64/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<X:8, Rest/binary>>) -> {X, Rest}.

varint(X) when X < 16#fd -> <<X>>;
varint(X) when X =< 16#ffff  -> <<16#fd, X:16/little>>;
varint(X) when X =< 16#ffffffff  -> <<16#fe, X:32/little>>;
varint(X) when X =< 16#ffffffffffffffff  -> <<16#ff, X:64/little>>.
