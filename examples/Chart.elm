module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import LineChart
import LineChart.Axis as Axis
import LineChart.Container as Container
import LineChart.Interpolation as Interpolation
import LineChart.Axis.Intersection as Intersection
import LineChart.Legends as Legends
import LineChart.Events as Events
import LineChart.Junk as Junk
import LineChart.Grid as Grid
import LineChart.Area as Area
import LineChart.Line as Line
import LineChart.Dots as Dots
import LineChart.Colors as Colors
import Zipper exposing (Zipper(..), extend)


maxDef : List Float -> Float -> Float
maxDef l d =
    case l of
        [] ->
            d

        x :: xs ->
            Basics.max d (List.foldr Basics.max x xs)


headDef : a -> List a -> a
headDef x xs =
    Maybe.withDefault x (List.head xs)


averageDef : Float -> List Float -> Float
averageDef d bs =
    case bs of
        [] ->
            d

        _ ->
            (List.sum bs) / (toFloat <| List.length bs)



---- Play with zippers ----


z : Zipper Float
z =
    Zipper
        [ 5, 9, 8, 5, 5, 5, 7, 8, 6, 9, 6, 10, 6, 9, 8, 6, 9, 7, 5, 8, 8, 8, 6, 6, 7 ]
        5
        [ 5, 9, 8, 8, 8, 9, 6, 7, 8, 5, 5, 7, 9, 8, 7, 7, 6, 6, 8, 6, 9, 9, 6, 7, 8 ]


maxSoFar : Zipper Float -> Float
maxSoFar (Zipper prev curr _) =
    maxDef prev curr


maximums : Zipper Float
maximums =
    extend maxSoFar z


peak : Zipper Float -> Float
peak (Zipper l f r) =
    if headDef f (List.reverse l) < f && f > headDef f r then
        10
    else
        0


peaks : Zipper Float
peaks =
    extend peak z


wma : Int -> Zipper Float -> Float
wma n (Zipper l f r) =
    averageDef f (List.take n (List.reverse l) ++ f :: List.take n r)



---- View ----


chartConfig : LineChart.Config { x : Float, y : Float } Never
chartConfig =
    { y = Axis.default 400 "Value" .y
    , x = Axis.default 1000 "Index" .x
    , container = Container.default "line-chart-1"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.normal 0.5
    , line = Line.default
    , dots = Dots.custom (Dots.empty 5 1)
    }


formatData : Zipper Float -> List { x : Float, y : Float }
formatData z =
    List.indexedMap (\i a -> { x = (toFloat i), y = a }) (Zipper.toList z)


chart : String -> ( String, Zipper Float ) -> ( String, Zipper Float ) -> Html Never
chart title ( lineTitleZ, z ) ( lineTitleModifiedZ, modifiedZ ) =
    let
        chartContainerStyles =
            [ ( "margin", "0 auto" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
    in
        div [ style chartContainerStyles ]
            [ h1 [] [ text title ]
            , LineChart.viewCustom chartConfig
                [ LineChart.line Colors.cyan Dots.square lineTitleZ (formatData z)
                , LineChart.line Colors.pinkLight Dots.diamond lineTitleModifiedZ (formatData modifiedZ)
                ]
            ]


main : Html Never
main =
    div []
        [ chart "extend maxSoFar z" ( "Z", z ) ( "Maximums", maximums )
        , chart "extend peak z" ( "Z", z ) ( "Peaks", peaks )
        , chart "extend (wma 3) z" ( "Z", z ) ( "Moving avg", extend (wma 3) z )
        , codeBlock "peak : Zipper Float -> Float\npeak (Zipper l f r) =\n    if headDef f (List.reverse l) < f && f > headDef f r then\n        10\n    else\n        0"
        , codeBlock "wma : Int -> Zipper Float -> Float\nwma n (Zipper l f r) =\n    averageDef f (List.take n (List.reverse l) ++ f :: List.take n r)\n"
        ]


codeBlock : String -> Html msg
codeBlock c =
    let
        styles =
            [ ( "text-align", "left" )
            , ( "padding-left", "20px" )
            ]
    in
        pre [ style styles ]
            [ code [] [ text c ] ]
