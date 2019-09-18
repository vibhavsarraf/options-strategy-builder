module Main exposing (..)

import List exposing (foldl)
import Html exposing (Html)
import LineChart


main : Html msg
main =
    LineChart.view1 .x .y (getPoints [trade1, trade2, trade3])


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

--type PnL = Profit Float | Loss Float

applyTradeType: TradeType -> Float -> Float
applyTradeType tt p =
    case tt of
        Buy -> p
        Sell -> -p

getPnL : Trade -> Float -> Float
getPnL (OptionTrade o tt q) expPrice =
    let
        profit =
            case o.type_ of
                Call ->
                    if expPrice < o.strike then -o.premium else ( expPrice - o.strike - o.premium )
                Put ->
                    if expPrice > o.strike then -o.premium else ( o.strike - expPrice - o.premium )
     in
     applyTradeType tt profit

getMultiPnL : List Trade -> Float -> Float
getMultiPnL lt expPrice = foldl (\t -> \v -> ( (getPnL t expPrice) + v )) 0 lt

trade1 = OptionTrade (Option Call 10800 96.1) Buy 75
trade2 = OptionTrade (Option Call 10900 42) Sell 75
trade3 = OptionTrade (Option Put 10700 13.6) Sell 75

--getExpiryPrice: Trade -> Float
--getExpiryPrice (OptionTrade)

getPoints : List Trade -> List Point
getPoints lt =
    [ Point 10600 (getMultiPnL lt 10600)
    , Point 10700 (getMultiPnL lt 10700)
    , Point 10800 (getMultiPnL lt 10800)
    , Point 10900 (getMultiPnL lt 10900)
    , Point 11000 (getMultiPnL lt 11000)]

