module Array.Extra exposing (..)

{-| Convenience functions for working with Array

# Transformations
@docs update, sliceFrom, sliceUntil, transpose

# Higher order helpers
@docs filterMap, apply, map2, map3, map4, map5, removeWhen

# Zips
@docs zip, zip3, zip4, zip5

# Slicing / resizing
@docs resizelRepeat, resizerRepeat, resizelIndexed, resizerIndexed, splitAt, removeAt

# Unsafe
@docs getUnsafe
-}

import Array exposing (..)
import Maybe
import Debug


{-| Update the element at the index using a function. Returns the array unchanged if the index is out of bounds.

    update  1 ((+)10) (fromList [1,2,3]) == fromList [1,12,3]
    update  4 ((+)10) (fromList [1,2,3]) == fromList [1,2,3]
    update -1 ((+)10) (fromList [1,2,3]) == fromList [1,2,3]
-}
update : Int -> (a -> a) -> Array a -> Array a
update n f a =
    let
        element =
            get n a
    in
        case element of
            Nothing ->
                a

            Just element' ->
                set n (f element') a


{-| Drop *n* first elements from an array. In other words, slice an array from an index until the very end. Given negative argument, count the end of the slice from the end of the array.

    sliceFrom  5 (fromList [0..9]) == fromList [5,6,7,8,9]
    sliceFrom -3 (fromList [0..9]) == fromList [7,8,9]
-}
sliceFrom : Int -> Array a -> Array a
sliceFrom n a =
    slice n (length a) a


{-| Take *n* first elements from an array. In other words, slice an array from the very beginning until index not including. Given negative argument, count the beginning of the slice from the end of the array.

    sliceUntil  5 (fromList [0..9]) == fromList [0,1,2,3,4]
    sliceUntil -3 (fromList [0..9]) == fromList [0,1,2,3,4,5,6]
-}
sliceUntil : Int -> Array a -> Array a
sliceUntil n a =
    if n >= 0 then
        slice 0 n a
    else
        slice 0 (length a + n) a


{-| Transpose an array of arrays.
-}
transpose : Array (Array a) -> Array (Array a)
transpose xss =
    let
        firstRow =
            get 0 xss
    in
        case firstRow of
            Nothing ->
                empty

            Just row ->
                foldl (map2 push) (repeat (length row) empty) xss


{-| Unsafe version of get, don't use this unless you know what you're doing!
-}
getUnsafe : Int -> Array a -> a
getUnsafe n xs =
    case get n xs of
        Just x ->
            x

        Nothing ->
            Debug.crash ("Index " ++ toString n ++ " of Array with length " ++ toString (length xs) ++ " is not reachable.")


{-| Apply a function that may succeed to all values in the array, but only keep the successes.

    String.toInt : String -> Maybe Int
    filterMap String.toInt (fromList ["3", "4.0", "5", "hats"]) == fromList [3,5]
-}
filterMap : (a -> Maybe b) -> Array a -> Array b
filterMap f xs =
    let
        maybePush : (a -> Maybe b) -> a -> Array b -> Array b
        maybePush f mx xs =
            case f mx of
                Just x ->
                    push x xs

                Nothing ->
                    xs
    in
        foldl (maybePush f) empty xs


{-| Apply an array of functions to an array of values.
-}
apply : Array (a -> b) -> Array a -> Array b
apply fs xs =
    let
        l =
            min (length fs) (length xs)

        fs' =
            slice 0 l fs
    in
        indexedMap (\n f -> f (getUnsafe n xs)) fs'


{-| Combine two arrays, combining them with the given function.
If one array is longer, the extra elements are dropped.

    map2 (+) [1,2,3] [1,2,3,4] == [2,4,6]
    map2 (,) [1,2,3] ['a','b'] == [ (1,'a'), (2,'b') ]
    pairs : Array a -> Array b -> Array (a,b)
    pairs lefts rights =
        map2 (,) lefts rights
-}
map2 : (a -> b -> result) -> Array a -> Array b -> Array result
map2 f ws =
    apply (map f ws)


{-| -}
map3 : (a -> b -> c -> result) -> Array a -> Array b -> Array c -> Array result
map3 f ws xs =
    apply (map2 f ws xs)


{-| -}
map4 : (a -> b -> c -> d -> result) -> Array a -> Array b -> Array c -> Array d -> Array result
map4 f ws xs ys =
    apply (map3 f ws xs ys)


{-| -}
map5 : (a -> b -> c -> d -> e -> result) -> Array a -> Array b -> Array c -> Array d -> Array e -> Array result
map5 f ws xs ys zs =
    apply (map4 f ws xs ys zs)


{-| Take a predicate and an array, return an array that contains elements which fails to satisfy the predicate.
    This is equivalent to `Array.filter (not << predicate) list`

    removeWhen isEven [1,2,3,4] == [1,3]
-}
removeWhen : (a -> Bool) -> Array a -> Array a
removeWhen pred xs =
    Array.filter (not << pred) xs


{-| Zip arrays into tuples
-}
zip : Array a -> Array b -> Array ( a, b )
zip =
    map2 (,)


{-| -}
zip3 : Array a -> Array b -> Array c -> Array ( a, b, c )
zip3 =
    map3 (,,)


{-| -}
zip4 : Array a -> Array b -> Array c -> Array d -> Array ( a, b, c, d )
zip4 =
    map4 (,,,)


{-| -}
zip5 : Array a -> Array b -> Array c -> Array d -> Array e -> Array ( a, b, c, d, e )
zip5 =
    map5 (,,,,)


{-| Resize an array from the left, padding the right-hand side with the given value.
-}
resizelRepeat : Int -> a -> Array a -> Array a
resizelRepeat n val xs =
    let
        l =
            length xs
    in
        if l > n then
            slice 0 n xs
        else if l < n then
            xs `append` repeat (n - l) val
        else
            xs


{-| Resize an array from the right, padding the left-hand side with the given value.
-}
resizerRepeat : Int -> a -> Array a -> Array a
resizerRepeat n val xs =
    let
        l =
            length xs
    in
        if l > n then
            slice (l - n) l xs
        else if l < n then
            repeat (n - l) val `append` xs
        else
            xs


{-| Resize an array from the left, padding the right-hand side with the given index function.
-}
resizelIndexed : Int -> (Int -> a) -> Array a -> Array a
resizelIndexed n f xs =
    let
        l =
            length xs

        gen m g =
            indexedMap (\i _ -> g i) <| repeat m ()
    in
        if l > n then
            slice 0 n xs
        else if l < n then
            xs `append` gen (n - l) (f << (\i -> i + l))
        else
            xs


{-| Resize an array from the right, padding the left-hand side with the given index function.
-}
resizerIndexed : Int -> (Int -> a) -> Array a -> Array a
resizerIndexed n f xs =
    let
        l =
            length xs

        gen m g =
            indexedMap (\i _ -> g i) <| repeat m ()
    in
        if l > n then
            slice (l - n) l xs
        else if l < n then
            gen (n - l) f `append` xs
        else
            xs


{-| Split an array into two arrays, the first ending at and the second starting at the given index
-}
splitAt : Int -> Array a -> ( Array a, Array a )
splitAt index xs =
    -- TODO: refactor (written this way to help avoid Array bugs)
    let
        len =
            length xs
    in
        case ( index > 0, index < len ) of
            ( True, True ) ->
                ( slice 0 index xs, slice index len xs )

            ( True, False ) ->
                ( xs, empty )

            ( False, True ) ->
                ( empty, xs )

            ( False, False ) ->
                ( empty, empty )


{-| Remove the element at the given index
-}
removeAt : Int -> Array a -> Array a
removeAt index xs =
    -- TODO: refactor (written this way to help avoid Array bugs)
    let
        ( xs0, xs1 ) =
            splitAt index xs

        len1 =
            length xs1
    in
        if len1 == 0 then
            xs0
        else
            xs0 `append` slice 1 len1 xs1
