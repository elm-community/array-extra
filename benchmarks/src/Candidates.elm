module Candidates exposing (..)

import Array exposing (Array)


reverseWithFoldlToList : Array a -> Array a
reverseWithFoldlToList =
    Array.foldl (::) [] >> Array.fromList


reverseWithFoldl : Array a -> Array a
reverseWithFoldl =
    Array.foldl Array.push Array.empty


reverseWithListReverse : Array a -> Array a
reverseWithListReverse =
    Array.toList >> List.reverse >> Array.fromList


indexedMapToListWithFoldr : (Int -> a -> b) -> Array a -> List b
indexedMapToListWithFoldr mapIndexAndValue array =
    Array.foldr
        (\x ( i, ys ) ->
            ( i - 1, mapIndexAndValue i x :: ys )
        )
        ( Array.length array - 1, [] )
        array
        |> Tuple.second


indexedMapToListWithToIndexedList : (Int -> b -> a) -> Array b -> List a
indexedMapToListWithToIndexedList mapIndexAndValue =
    Array.toIndexedList
        >> List.map (\( i, v ) -> mapIndexAndValue i v)


indexedMapToListWithListIndexedMap : (Int -> a -> b) -> Array a -> List b
indexedMapToListWithListIndexedMap mapIndexAndValue =
    Array.toList >> List.indexedMap mapIndexAndValue


indexedMapToListWithArrayIndexedMap : (Int -> a -> b) -> Array a -> List b
indexedMapToListWithArrayIndexedMap mapIndexAndValue =
    Array.indexedMap mapIndexAndValue >> Array.toList


mapToListWithFoldr : (a -> b) -> Array a -> List b
mapToListWithFoldr alter =
    Array.foldr (alter >> (::)) []


mapToListWithListMap : (a -> b) -> Array a -> List b
mapToListWithListMap alter =
    Array.toList >> List.map alter


unzipWithMaps : Array ( a, b ) -> ( Array a, Array b )
unzipWithMaps tupleArray =
    ( tupleArray |> Array.map Tuple.first
    , tupleArray |> Array.map Tuple.second
    )


unzipWithListUnzip : Array ( a, b ) -> ( Array a, Array b )
unzipWithListUnzip tupleArray =
    tupleArray
        |> Array.toList
        |> List.unzip
        |> Tuple.mapBoth Array.fromList Array.fromList


unzipWithFoldlToArrays : Array ( a, b ) -> ( Array a, Array b )
unzipWithFoldlToArrays arrAB =
    Array.foldl
        (\( a, b ) ( arrA, arrB ) ->
            ( Array.push a arrA, Array.push b arrB )
        )
        ( Array.empty, Array.empty )
        arrAB


map2WithListMap2 : (a -> b -> c) -> Array a -> Array b -> Array c
map2WithListMap2 combine aArray bArray =
    List.map2 combine
        (Array.toList aArray)
        (Array.toList bArray)
        |> Array.fromList


map2WithListIndexedMap : (a -> b -> c) -> Array a -> Array b -> Array c
map2WithListIndexedMap combine aArray bArray =
    let
        length =
            min (Array.length aArray) (Array.length bArray)

        aList =
            Array.slice 0 length aArray
                |> Array.toList
    in
    aList
        |> List.indexedMap
            (\i a ->
                Maybe.map (combine a) (Array.get i bArray)
            )
        |> List.filterMap identity
        |> Array.fromList


filterMapWithPush : (a -> Maybe b) -> Array a -> Array b
filterMapWithPush f xs =
    let
        maybePush : (a -> Maybe b) -> a -> Array b -> Array b
        maybePush f_ mx xs_ =
            case f_ mx of
                Just x ->
                    Array.push x xs_

                Nothing ->
                    xs_
    in
    Array.foldl (maybePush f) Array.empty xs


filterMapWithListFilterMap : (a -> Maybe mapped) -> Array a -> Array mapped
filterMapWithListFilterMap tryMap =
    Array.toList
        >> List.filterMap tryMap
        >> Array.fromList
