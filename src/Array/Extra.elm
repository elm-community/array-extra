module Array.Extra exposing
    ( filterMap, apply, mapToList, indexedMapToList, map2, map3, map4, map5, removeWhen, reverse
    , zip, zip3, unzip
    , sliceFrom, sliceUntil, resizelRepeat, resizerRepeat, resizelIndexed, resizerIndexed, splitAt
    , removeAt, insertAt, pop, update
    )

{-| Convenience functions for working with Array


# Transform

@docs filterMap, apply, mapToList, indexedMapToList, map2, map3, map4, map5, removeWhen, reverse


# Zip

@docs zip, zip3, unzip


# Slice / resize

@docs sliceFrom, sliceUntil, resizelRepeat, resizerRepeat, resizelIndexed, resizerIndexed, splitAt


# Modify

@docs removeAt, insertAt, pop, update

-}

import Array exposing (..)


{-| Update the element at the index using a function. Returns the array unchanged if the index is out of bounds.

    update 1 ((+) 10) (fromList [ 1, 2, 3 ]) == fromList [ 1, 12, 3 ]

    update 4 ((+) 10) (fromList [ 1, 2, 3 ]) == fromList [ 1, 2, 3 ]

    update -1 ((+) 10) (fromList [ 1, 2, 3 ]) == fromList [ 1, 2, 3 ]

-}
update : Int -> (a -> a) -> Array a -> Array a
update n f a =
    let
        element =
            Array.get n a
    in
    case element of
        Nothing ->
            a

        Just element_ ->
            Array.set n (f element_) a


{-| Drop _n_ first elements from an array. In other words, slice an array from an index until the very end. Given negative argument, count the end of the slice from the end of the array.

    sliceFrom 5 (fromList (List.range 0 9)) == fromList [ 5, 6, 7, 8, 9 ]

    sliceFrom -3 (fromList (List.range 0 9)) == fromList [ 7, 8, 9 ]

-}
sliceFrom : Int -> Array a -> Array a
sliceFrom n a =
    slice n (length a) a


{-| Take _n_ first elements from an array. In other words, slice an array from the very beginning until index not including. Given negative argument, count the beginning of the slice from the end of the array.

    sliceUntil 5 (fromList (List.range 0 9)) == fromList [ 0, 1, 2, 3, 4 ]

    sliceUntil -3 (fromList (List.range 0 9)) == fromList [ 0, 1, 2, 3, 4, 5, 6 ]

-}
sliceUntil : Int -> Array a -> Array a
sliceUntil n a =
    if n >= 0 then
        slice 0 n a

    else
        slice 0 (length a + n) a


{-| Remove the last element from an array.

    pop (fromList [ 1, 2, 3 ]) == fromList [ 1, 2 ]

-}
pop : Array a -> Array a
pop arr =
    slice 0 -1 arr


{-| Apply a function that may succeed to all values in the array, but only keep the successes.

    String.toInt : String -> Maybe Int
    filterMap String.toInt (fromList [ "3", "4.0", "5", "hats" ]) == fromList [ 3, 5 ]

-}
filterMap : (a -> Maybe b) -> Array a -> Array b
filterMap f xs =
    xs
        |> Array.toList
        |> List.filterMap f
        |> Array.fromList


{-| Apply an array of functions to an array of values.
-}
apply : Array (a -> b) -> Array a -> Array b
apply fs xs =
    map2 (\f b -> f b) fs xs


{-| Apply a function to the array, collecting the result in a List.
This is useful for building HTML out of an array:

    Html.text : String -> Html msg
    mapToList Html.text : Array String -> List (Html msg)

-}
mapToList : (a -> b) -> Array a -> List b
mapToList f =
    Array.foldr (f >> (::)) []


{-| Apply a function to the array with the index as the first argument,
collecting the results in a List.

    type alias Exercise =
        { name : String }

    renderExercise : Int -> Exercise -> Html msg
    renderExercise index exercise =
        String.concat
            [ "Exercise #"
            , String.fromInt (index + 1)
            , " - "
            , exercise.name
            ]
            |> Html.text

    renderExercises : Array Exercise -> Html msg
    renderExercises exercises =
        indexedMapToList renderExercise exercises
            |> Html.div []

-}
indexedMapToList : (Int -> a -> b) -> Array a -> List b
indexedMapToList f xs =
    Array.foldr
        (\x ( i, ys ) -> ( i - 1, f i x :: ys ))
        ( Array.length xs - 1, [] )
        xs
        |> Tuple.second


{-| Combine two arrays, combining them with the given function.
If one array is longer, the extra elements are dropped.

    map2 (+) [ 1, 2, 3 ] [ 1, 2, 3, 4 ] == [ 2, 4, 6 ]
    map2 Tuple.pair [ 1, 2, 3 ] [ 'a', 'b' ] == [ ( 1, 'a' ), ( 2, 'b' ) ]

    pairs : Array a -> Array b -> Array ( a, b )
    pairs lefts rights =
        map2 Tuple.pair lefts rights

-}
map2 : (a -> b -> result) -> Array a -> Array b -> Array result
map2 f ws =
    Array.toList
        >> List.map2 f (ws |> Array.toList)
        >> Array.fromList


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

    removeWhen isEven (fromList [ 1, 2, 3, 4 ]) == fromList [ 1, 3 ]

-}
removeWhen : (a -> Bool) -> Array a -> Array a
removeWhen pred xs =
    Array.filter (not << pred) xs


{-| Zip arrays into tuples.
-}
zip : Array a -> Array b -> Array ( a, b )
zip =
    map2 Tuple.pair


{-| -}
zip3 : Array a -> Array b -> Array c -> Array ( a, b, c )
zip3 =
    map3 (\a b c -> ( a, b, c ))


{-| Unzip an array of tuples into a tuple containing two arrays for the values first & the second in the tuples.

    unzip (fromList [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ])
        == ( fromList [ 1, 2, 3 ]
           , fromList [ 'a', 'b', 'c' ]
           )

-}
unzip : Array ( a, b ) -> ( Array a, Array b )
unzip arrAB =
    ( arrAB |> Array.map Tuple.first
    , arrAB |> Array.map Tuple.second
    )


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
        append xs (repeat (n - l) val)

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
        append (repeat (n - l) val) xs

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
        append xs (gen (n - l) (f << (\i -> i + l)))

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
        append (gen (n - l) f) xs

    else
        xs


{-| Reverse an array.

    reverse (fromList [ 1, 2, 3 ]) == fromList [ 3, 2, 1 ]

-}
reverse : Array a -> Array a
reverse xs =
    xs
        |> Array.toList
        |> List.reverse
        |> Array.fromList


{-| Split an array into two arrays, the first ending at and the second starting at the given index.

    splitAt 2 (fromList [ 1, 2, 3, 4 ])
        == ( fromList [ 1, 2 ], fromList [ 3, 4 ] )

    splitAt -1 (fromList [ 1, 2, 3, 4 ])
        == ( empty, fromList [ 1, 2, 3, 4 ] )

    splitAt 100 (fromList [ 1, 2, 3, 4 ])
        == ( fromList [ 1, 2, 3, 4 ], empty )

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


{-| Remove the element at the given index.

    removeAt 2 (fromList [ 1, 2, 3, 4 ])
        == fromList [ 1, 2, 4 ]

    removeAt -1 (fromList [ 1, 2, 3, 4 ])
        == fromList [ 1, 2, 3, 4 ]

    removeAt 100 (fromList [ 1, 2, 3, 4 ])
        == fromList [ 1, 2, 3, 4 ]

-}
removeAt : Int -> Array a -> Array a
removeAt index xs =
    -- TODO: refactor (written this way to help avoid Array bugs)
    let
        ( beforeIndex, startingAtIndex ) =
            splitAt index xs

        lenStartingAtIndex =
            length startingAtIndex
    in
    if length beforeIndex == 0 then
        startingAtIndex

    else if lenStartingAtIndex == 0 then
        beforeIndex

    else
        append beforeIndex
            (slice 1 lenStartingAtIndex startingAtIndex)


{-| Insert an element at the given index.

    insertAt 0 'b' (fromList [ 'a', 'c' ]) == fromList [ 'b', 'a', 'c' ]

    insertAt 1 'b' (fromList [ 'a', 'c' ]) == fromList [ 'a', 'b', 'c' ]

    insertAt -1 'b' (fromList [ 'a', 'c' ]) == fromList [ 'a', 'c' ]

    insertAt 10 'b' (fromList [ 'a', 'c' ]) == fromList [ 'a', 'c' ]

-}
insertAt : Int -> a -> Array a -> Array a
insertAt index val values =
    let
        length =
            Array.length values
    in
    if index >= 0 && index <= length then
        let
            before =
                Array.slice 0 index values

            after =
                Array.slice index length values
        in
        Array.append (Array.push val before) after

    else
        values
