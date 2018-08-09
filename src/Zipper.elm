module Zipper
    exposing
        ( Zipper(..)
        , prev
        , next
        , map
        , leftMay
        , rightMay
        , left
        , right
        , extract
        , duplicate
        , extend
        , toList
        , append
        , prepend
        )

{-| This package provides an implementation of a [List Zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)) as well as its corresponding comonadic interface, namely, the functions [`extract`](#extract), [`duplicate`](#duplicate) and [`extend`](#extend).

These are useful to perform transformations which depend on the neighborhood of the elements, see [`extend`](#extend) for one such example.

You can find more complete examples [here](https://github.com/jjant/list-zipper-comonad-elm/tree/master/examples).

@docs Zipper, append, prepend, toList


## Extracting values

@docs prev, extract, next


## Navigating

@docs leftMay, rightMay, left, right


## Transforming elements

@docs map, duplicate, extend

-}


{-| A `Zipper a` is a nonempty list with a focused element.
-}
type Zipper a
    = Zipper (List a) a (List a)


{-| Returns the elements before the focused element.

      import Zipper exposing (Zipper(..))

      Zipper.prev (Zipper [0] 1 [2,3,4]) == [0]

-}
prev : Zipper a -> List a
prev (Zipper p _ _) =
    p


{-| Returns the elements after the focused element.

      import Zipper exposing (Zipper(..))

      Zipper.next (Zipper [0] 1 [2,3,4]) == [2,3,4]

-}
next : Zipper a -> List a
next (Zipper _ _ n) =
    n


{-| Attempts to select the element to the left of the focus, returns `Nothing` if there are no previous elements.

      import Zipper exposing (Zipper(..))

      Zipper.leftMay (Zipper [] 1 [2,3,4]) == Nothing

      Zipper.leftMay (Zipper [1] 2 [3,4]) == Just (Zipper [] 1 [2, 3, 4])

-}
leftMay : Zipper a -> Maybe (Zipper a)
leftMay (Zipper p a n) =
    case List.reverse p of
        [] ->
            Nothing

        x :: xs ->
            Just <| Zipper (List.reverse xs) x (a :: n)


{-| Attempts to select the element to the right of the focus, returns `Nothing` if there are no next elements.

    import Zipper exposing (Zipper(..))

    Zipper.rightMay (Zipper [0] 1 []) == Nothing

    Zipper.rightMay (Zipper [1] 2 [3,4]) == Just (Zipper [1, 2] 3 [4])

-}
rightMay : Zipper a -> Maybe (Zipper a)
rightMay (Zipper p a n) =
    case n of
        [] ->
            Nothing

        x :: xs ->
            Just <| Zipper (p ++ [ a ]) x xs


{-| Attempts to select the element to the left of the focus. Returns the Zipper unchanged if there are no previous elements.

      import Zipper exposing (Zipper(..))

      Zipper.left (Zipper [] 1 [2,3,4]) == Zipper [] 1 [2,3,4]

      Zipper.left (Zipper [1] 2 [3,4]) == Zipper [] 1 [2, 3, 4]

-}
left : Zipper a -> Zipper a
left z =
    leftMay z |> Maybe.withDefault z


{-| Attempts to select the element to the right of the focus. Returns the Zipper unchanged if there are no next elements.

    import Zipper exposing (Zipper(..))

    Zipper.right (Zipper [0] 1 []) == Zipper [0] 1 []

    Zipper.right (Zipper [1] 2 [3,4]) == Zipper [1, 2] 3 [4]

-}
right : Zipper a -> Zipper a
right z =
    rightMay z |> Maybe.withDefault z



----- Zipper Functor instance


{-| Apply a function to every element in the Zipper.

    import Zipper exposing (Zipper(..))

    Zipper.map (\x -> 2 * x) (Zipper [2, 0, 5] 1 [2]) == Zipper [4, 0, 10] 2 [4]

-}
map : (a -> b) -> Zipper a -> Zipper b
map f (Zipper p a n) =
    Zipper (List.map f p) (f a) (List.map f n)



----- Zipper Comonad instance (extract, duplicate, extend)


{-| Returns the focused element.

    import Zipper exposing (Zipper(..))

    Zipper.extract (Zipper [0] 1 [2,3,4]) == 1

-}
extract : Zipper a -> a
extract (Zipper _ a _) =
    a


{-| Returns a Zipper in which every element is a Zipper list.

    import Zipper exposing (Zipper(..))

    duplicate (Zipper [0] 1 [2]) == Zipper [Zipper [] 0 [1,2]] (Zipper [0] 1 [2]) [Zipper [0, 1] 2 []]

-}
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


{-| Map elements in the Zipper, having access to the elements before and after it.

    import Zipper exposing (Zipper(..))

    maxSoFar (Zipper prev curr _) = List.foldr Basics.max curr prev

    Zipper.extend maxSoFar (Zipper [2, 1, 3, 4, 5] 1 [2]) == Zipper [2, 2, 3, 4, 5] 5 [5]

-}
extend : (Zipper a -> b) -> Zipper a -> Zipper b
extend f =
    map f << duplicate



----- Util


{-| Transforms a Zipper into a regular list.

    import Zipper exposing (Zipper(..))

    Zipper.toList (Zipper [0] 1 [2,3,4]) == [0,1,2,3,4]

-}
toList : Zipper a -> List a
toList (Zipper p a n) =
    p ++ (a :: n)


{-| Add elements at the end of the Zipper.

    import Zipper exposing (Zipper(..))

    Zipper.append [ 5, 4 ] (Zipper [ 0 ] 1 [ 2, 3 ]) == Zipper [ 0 ] 1 [ 2, 3, 5, 4 ]

-}
append : List a -> Zipper a -> Zipper a
append xs (Zipper p c n) =
    Zipper p c (n ++ xs)


{-| Add elements at the beginning of the Zipper.

    import Zipper exposing (Zipper(..))

    Zipper.prepend [ -1, -2 ] (Zipper [ 0 ] 1 [ 2, 3 ]) == Zipper [ -1, -2, 0 ] 1 [ 2, 3 ]

-}
prepend : List a -> Zipper a -> Zipper a
prepend xs (Zipper p c n) =
    Zipper (xs ++ p) c n



---- PRIVATE UTILITY


iterate : (a -> Maybe a) -> a -> List a
iterate f x =
    case f x of
        Just x_ ->
            x :: iterate f x_

        Nothing ->
            [ x ]
