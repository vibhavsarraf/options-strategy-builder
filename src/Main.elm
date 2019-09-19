module Main exposing (..)

import Html exposing (Html, div)
import LineChart
import List exposing (foldl)
import Svg exposing (Svg)


trades =
    [ OptionTrade (Option Call 10600 158.9) Buy 75
    , OptionTrade (Option Call 10800 52.8) Sell 75
    , OptionTrade (Option Put 10500 24.65) Sell 75
    ]


main : Html msg
main =
    div []
        [ getPnLChart trades
        ]


type alias Point =
    { x : Float, y : Float }


type OptionType
    = Call
    | Put


type TradeType
    = Buy
    | Sell


type alias Quantity =
    Int


type alias Option =
    { type_ : OptionType
    , strike : Float
    , premium : Float
    }


type Trade
    = OptionTrade Option TradeType Quantity


getPnL : Trade -> Float -> Float
getPnL (OptionTrade o tt q) expPrice =
    let
        profitPerStock =
            case o.type_ of
                Call ->
                    if expPrice < o.strike then
                        -o.premium

                    else
                        expPrice - o.strike - o.premium

                Put ->
                    if expPrice > o.strike then
                        -o.premium

                    else
                        o.strike - expPrice - o.premium

        profit =
            profitPerStock * toFloat q
    in
    case tt of
        Buy ->
            profit

        Sell ->
            -profit


getMultiPnL : List Trade -> Float -> Float
getMultiPnL lt expPrice =
    foldl (\t -> \v -> getPnL t expPrice + v) 0 lt


getStrikePrice : Trade -> Float
getStrikePrice (OptionTrade o tt q) =
    o.strike


computeXPoints : List Trade -> List Float
computeXPoints =
    addExtraBack << addExtraFront 0.99 << List.sort << List.map getStrikePrice


addExtraFront : Float -> List Float -> List Float
addExtraFront factor lx =
    case lx of
        [] ->
            []

        x :: xs ->
            (x * factor) :: (x :: xs)


addExtraBack : List Float -> List Float
addExtraBack =
    List.reverse << addExtraFront 1.01 << List.reverse


getPoints : List Trade -> List Point
getPoints lt =
    (List.map (\x -> Point x (getMultiPnL lt x)) << computeXPoints) lt


getPnLChart : List Trade -> Svg msg
getPnLChart lt =
    LineChart.view1 .x .y (getPoints lt)
