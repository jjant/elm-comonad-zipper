module Main exposing (..)

import Html exposing (..)
import Html.Attributes
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


iterate : (a -> Maybe a) -> a -> List a
iterate f x =
    case f x of
        Just x_ ->
            x :: iterate f x_

        Nothing ->
            [ x ]



----- Zipper type and accessors


type Zipper a
    = Zipper (List a) a (List a)


prev : Zipper a -> List a
prev (Zipper p _ _) =
    p


next : Zipper a -> List a
next (Zipper _ _ n) =
    n


leftMay : Zipper a -> Maybe (Zipper a)
leftMay (Zipper p a n) =
    case List.reverse p of
        [] ->
            Nothing

        x :: xs ->
            Just <| Zipper (List.reverse xs) x (a :: n)


rightMay : Zipper a -> Maybe (Zipper a)
rightMay (Zipper p a n) =
    case n of
        [] ->
            Nothing

        x :: xs ->
            Just <| Zipper (p ++ [ a ]) x xs


left : Zipper a -> Zipper a
left z =
    leftMay z |> Maybe.withDefault z


right : Zipper a -> Zipper a
right z =
    rightMay z |> Maybe.withDefault z



----- Zipper Functor instance


map : (a -> b) -> Zipper a -> Zipper b
map f (Zipper p a n) =
    Zipper (List.map f p) (f a) (List.map f n)



----- Zipper Comonad instance (extract, duplicate, extend)


extract : Zipper a -> a
extract (Zipper _ a _) =
    a


duplicate : Zipper a -> Zipper (Zipper a)
duplicate ((Zipper prev curr next) as z) =
    let
        lefts =
            List.reverse <| gather leftMay z

        rights =
            gather rightMay z

        gather : (b -> Maybe b) -> b -> List b
        gather f =
            List.drop 1 << iterate f
    in
        Zipper lefts z rights


extend : (Zipper a -> b) -> Zipper a -> Zipper b
extend f =
    map f << duplicate



----- Play with zippers


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


maxSoFar : Zipper Float -> Float
maxSoFar (Zipper prev curr _) =
    maxDef prev curr


peak : Zipper Float -> Float
peak (Zipper l f r) =
    if headDef f (List.reverse l) < f && f > headDef f r then
        10
    else
        0


wma : Int -> Zipper Float -> Float
wma n (Zipper l f r) =
    averageDef f (List.take n (List.reverse l) ++ f :: List.take n r)



----- Util


toList : Zipper a -> List a
toList (Zipper p a n) =
    p ++ (a :: n)



------ Chart


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
    List.indexedMap (\i a -> { x = (toFloat i), y = a }) (toList z)


z : Zipper Float
z =
    Zipper [ 5, 9, 8, 5, 5, 5, 7, 8, 6, 9, 6, 10, 6, 9, 8, 6, 9, 7, 5, 8, 8, 8, 6, 6, 7 ] 5 [ 5, 9, 8, 8, 8, 9, 6, 7, 8, 5, 5, 7, 9, 8, 7, 7, 6, 6, 8, 6, 9, 9, 6, 7, 8 ]


maximums : Zipper Float
maximums =
    extend maxSoFar z


peaks : Zipper Float
peaks =
    extend peak z


main : Html Never
main =
    div []
        [ div []
            [ h1 [] [ text "extend maxSoFar z" ]
            , LineChart.viewCustom chartConfig
                [ LineChart.line Colors.cyan Dots.square "Z" (formatData z)
                , LineChart.line Colors.pinkLight Dots.diamond "Maximums" (formatData maximums)
                ]
            ]
        , div []
            [ h1 [] [ text "extend peak z" ]
            , LineChart.viewCustom chartConfig
                [ LineChart.line Colors.cyan Dots.square "Z" (formatData z)
                , LineChart.line Colors.pinkLight Dots.diamond "Peaks" (formatData peaks)
                ]
            ]
        , div []
            [ h1 [] [ text "extend (wma 3) z" ]
            , LineChart.viewCustom chartConfig
                [ LineChart.line Colors.cyan Dots.square "Z" (formatData z)
                , LineChart.line Colors.pinkLight Dots.diamond "Moving avg" (formatData (extend (wma 3) z))
                ]
            ]
        , div []
            [ pre
                [ Html.Attributes.style
                    [ ( "text-align", "left" )
                    , ( "padding-left", "20px" )
                    ]
                ]
                [ code []
                    [ text "peak : Zipper Float -> Float\npeak (Zipper l f r) =\n    if headDef f (List.reverse l) < f && f > headDef f r then\n        10\n    else\n        0" ]
                ]
            , pre
                [ Html.Attributes.style
                    [ ( "text-align", "left" )
                    , ( "padding-left", "20px" )
                    ]
                ]
                [ code []
                    [ text "wma : Int -> Zipper Float -> Float\nwma n (Zipper l f r) =\n    averageDef f (List.take n (List.reverse l) ++ f :: List.take n r)\n" ]
                ]
            ]
        ]
