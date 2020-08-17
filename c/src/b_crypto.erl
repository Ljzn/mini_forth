%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Common bitcoin crypto functions
%%% @end
%%% --------------------------------------------------------------------------

-module(b_crypto).

-export([pf_inv/1, pf_mul/2]).

%% EC secp256k1 curve

%% Prime field order
-define(P, 16#fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f).

pf_mul(X, Y) ->
    (X*Y) rem ?P.

%% Inversion using the extended Euclidean algorithm
%% Algorithm 2.20 from "Guide to Elliptic Cruve Cryptography"
%% inefficient, but simple
pf_inv(X) ->
    pf_inv(X, ?P).

pf_inv(X, P) ->
    (pf_inv(X, P, 1, 0) + P) rem P.

pf_inv(U, _V, X1, _X2) when U =:= 1 ->
    X1;

pf_inv(U, V, X1, X2) when U > 1 ->
    Q = V div U,
    pf_inv(V - Q*U, U, X2 - Q*X1, X1).
