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
