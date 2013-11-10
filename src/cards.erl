-module(cards).
-export([kind/1, main/0, suit/1]).

-type suit()  :: spades | clubs | hearts | diamonds.
-type value() :: 1..10 | jack | queen | king.
-type card()  :: {suit(), value()}.

-spec kind(card()) -> face | number.
kind({_, A}) when A >= 1, A =< 10 -> number;
kind(_) -> face.

-spec suit(card()) -> suit().
suit({Suit, _}) -> Suit.

main() ->
    number = kind({spades, 7}),
    face   = kind({hearts, king}),
    number = kind({rubies, 4}),
    face   = kind({clubs, queen}).
