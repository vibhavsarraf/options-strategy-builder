module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, br, div, input, option, select, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Color
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line
import List exposing (foldl, foldr)
import Svg exposing (Svg)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { trades : List TradeHtml
    , tradeType : String
    , optionType : String
    , price : String
    , strike : String
    , quantity : String
    , errMsg : Maybe String
    , id : Int
    }


init : Model
init =
    { trades =
        [ { trade = OptionTrade (Option Call 10600 158.9) Buy 75, active = True, id = 0 }
        , { trade = OptionTrade (Option Call 10800 52.5) Sell 75, active = True, id = 1 }
        ]
    , price = "10.3"
    , strike = "10500"
    , quantity = "75"
    , errMsg = Nothing
    , optionType = "call"
    , tradeType = "buy"
    , id = 2
    }


type Msg
    = Price String
    | Strike String
    | Quantity String
    | TradeType String
    | OptionType String
    | AddTrade
    | DeleteTrade Int
    | FlipActive Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Price p ->
            { model | price = p }

        Strike s ->
            { model | strike = s }

        Quantity q ->
            { model | quantity = q }

        TradeType tt ->
            { model | tradeType = tt }

        OptionType ot ->
            { model | optionType = ot }

        AddTrade ->
            addTrade model

        DeleteTrade id ->
            { model | trades = List.filter (not << checkIdMatch id) model.trades }

        FlipActive id ->
            { model | trades = List.map (flipActive id) model.trades }


view : Model -> Html Msg
view model =
    div []
        [ (getPnLChart << getActiveTrades) model
        , (div [] << List.map convTradeHtml << .trades) model
        , br [] []
        , getSelect [ buySelectOption, sellSelectOption ] TradeType
        , getSelect [ callSelectOption, putSelectOption ] OptionType
        , viewInput "text" "Strike" model.strike Strike
        , viewInput "text" "Price" model.price Price
        , viewInput "text" "Quantity" model.quantity Quantity
        , input [ type_ "submit", value "Add", onClick AddTrade ] []
        , br [] []
        , getErrorText model

        --        , text (toString model)
        ]


flipActive : Int -> TradeHtml -> TradeHtml
flipActive id t =
    if t.id == id then
        { t | active = not t.active }

    else
        t


checkIdMatch : Int -> TradeHtml -> Bool
checkIdMatch id t =
    t.id == id


getErrorText : Model -> Html msg
getErrorText model =
    case model.errMsg of
        Just err ->
            text err

        Nothing ->
            text ""



-- This style is bad. How to do it in a better way?


addTrade : Model -> Model
addTrade model =
    case String.toFloat model.strike of
        Just s ->
            case String.toFloat model.price of
                Just p ->
                    case String.toInt model.quantity of
                        Just q ->
                            case stringToOptionType model.optionType of
                                Just ot ->
                                    case stringToTradeType model.tradeType of
                                        Just tt ->
                                            { model
                                                | trades = model.trades ++ [ TradeHtml (OptionTrade (Option ot s p) tt q) True model.id ]
                                                , id = model.id + 1
                                                , errMsg = Nothing
                                            }

                                        Nothing ->
                                            putWrongInputError model "Buy or Sell"

                                Nothing ->
                                    putWrongInputError model "option type"

                        Nothing ->
                            putWrongInputError model "quantity"

                Nothing ->
                    putWrongInputError model "price"

        Nothing ->
            putWrongInputError model "strike price"


putWrongInputError : Model -> String -> Model
putWrongInputError model s =
    { model | errMsg = Just ("Wrong input " ++ s) }


stringToTradeType : String -> Maybe TradeType
stringToTradeType s =
    case s of
        "buy" ->
            Just Buy

        "sell" ->
            Just Sell

        _ ->
            Nothing


stringToOptionType : String -> Maybe OptionType
stringToOptionType s =
    case s of
        "call" ->
            Just Call

        "put" ->
            Just Put

        _ ->
            Nothing


type alias SelectOption =
    { value : String
    , text_ : String
    }


buySelectOption : SelectOption
buySelectOption =
    { value = "buy"
    , text_ = "Buy"
    }


sellSelectOption : SelectOption
sellSelectOption =
    { value = "sell"
    , text_ = "Sell"
    }


callSelectOption : SelectOption
callSelectOption =
    { value = "call"
    , text_ = "Call"
    }


putSelectOption : SelectOption
putSelectOption =
    { value = "put"
    , text_ = "Put"
    }


getOptionView : SelectOption -> Html msg
getOptionView so =
    option [ value so.value ] [ text so.text_ ]


getSelect : List SelectOption -> (String -> msg) -> Html msg
getSelect lo toMsg =
    (select [ onInput toMsg ] << List.map getOptionView) lo


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


getActiveTrades : Model -> List Trade
getActiveTrades =
    List.map .trade << List.filter .active << .trades


getTradeType : Trade -> TradeType
getTradeType (OptionTrade _ tt _) =
    tt


getTradeQuantity : Trade -> Quantity
getTradeQuantity (OptionTrade _ _ q) =
    q


getTradeOption : Trade -> Option
getTradeOption (OptionTrade o _ _) =
    o


convTradeHtml : TradeHtml -> Html Msg
convTradeHtml th =
    let
        attributes =
            case th.active of
                True ->
                    []

                False ->
                    [ style "text-decoration" "line-through" ]
    in
    div attributes
        [ text
            (foldr (++)
                ""
                [ (toString << getTradeType) th.trade
                , " "
                , (toString << getTradeQuantity) th.trade
                , " "
                , (toString << .type_ << getTradeOption) th.trade
                , " "
                , (toString << .strike << getTradeOption) th.trade
                , " at "
                , (toString << .premium << getTradeOption) th.trade
                ]
            )
        , input
            [ type_ "submit"
            , value
                (if th.active then
                    "Hide"

                 else
                    "Unhide"
                )
            , onClick (FlipActive th.id)
            ]
            []
        , input [ type_ "submit", value "Remove", onClick (DeleteTrade th.id) ] []
        ]


type alias TradeHtml =
    { trade : Trade
    , active : Bool
    , id : Int
    }


type alias Point =
    { expPrice : Float
    , profit : Float
    }


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
    LineChart.viewCustom
        { y = Axis.default 450 "Profit" .profit
        , x = Axis.default 700 "Expiry Price" .expPrice
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.default
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }
        [ LineChart.line Color.green Dots.circle "Strategy" (getPoints lt) ]



--    LineChart.view1 .expPrice .profit (getPoints lt)
