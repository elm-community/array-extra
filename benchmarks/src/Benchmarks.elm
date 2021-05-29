module Benchmarks exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import Candidates exposing (..)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    describe "array extra"
        [ compare "from 0 to 99"
            (\f -> f ())
            ( "initialize"
            , \() -> Array.initialize 100 (\i -> i)
            )
            ( "fromList (List.range 0 _)"
            , \() -> Array.fromList (List.range 0 99)
            )
        , compare "mapToList"
            (\mapToList -> mapToList (\v -> -v) ints1To100)
            ( "with foldr", mapToListWithFoldr )
            ( "with Array.toIndexedList", mapToListWithListMap )
        , let
            compareToWithFoldr ( description, function ) =
                compare (description ++ " vs with Array.foldr")
                    (\indexedMapToList ->
                        indexedMapToList Tuple.pair
                            ints1To100
                    )
                    ( "with Array.foldr"
                    , indexedMapToListWithFoldr
                    )
                    ( description, function )
          in
          describe "indexedMapToList"
            [ compareToWithFoldr
                ( "with toIndexedList"
                , indexedMapToListWithToIndexedList
                )
            , compareToWithFoldr
                ( "with Array.indexedMap"
                , indexedMapToListWithArrayIndexedMap
                )
            , compareToWithFoldr
                ( "with List.indexedMap"
                , indexedMapToListWithListIndexedMap
                )
            ]
        , let
            compareToFoldlToList ( description, function ) =
                compare
                    (description ++ " vs with Array.foldl to list")
                    (\reverse -> reverse ints1To100)
                    ( "with Array.foldl to list", reverseWithFoldlToList )
                    ( description, function )
          in
          describe "reverse"
            [ compareToFoldlToList
                ( "with List.reverse", reverseWithListReverse )
            , compareToFoldlToList
                ( "with Array.foldr to array", reverseWithFoldl )
            ]
        , let
            zipped =
                Array.zip
                    ints1To100
                    ints1To100

            compareToWithArrayMaps ( description, function ) =
                compare "vs with Array.maps"
                    (\unzip -> unzip zipped)
                    ( "with Array.maps", unzipWithMaps )
                    ( description, function )
          in
          describe "unzip"
            [ compareToWithArrayMaps
                ( "with List.unzip", unzipWithListUnzip )
            , compareToWithArrayMaps
                ( "with foldl to Arrays", unzipWithFoldlToArrays )
            ]
        , compare "map2"
            (\map2 ->
                map2 Tuple.pair
                    ints1To100
                    ints1To100
            )
            ( "with List.map2", map2WithListMap2 )
            ( "with List.indexedMap", map2WithListIndexedMap )
        , let
            maybeInts =
                Array.initialize 100
                    (\x ->
                        if (x |> modBy 3) == 0 then
                            Nothing

                        else
                            Just x
                    )
          in
          compare "filterMap"
            (\filterMap -> filterMap identity maybeInts)
            ( "with List.filterMap", filterMapWithListFilterMap )
            ( "with push", filterMapWithPush )
        ]


compare :
    String
    -> (a -> b_)
    -> ( String, a )
    -> ( String, a )
    -> Benchmark
compare name applyArguments ( aDescription, aFunction ) ( bDescription, bFunction ) =
    Benchmark.compare name
        aDescription
        (\() -> applyArguments aFunction)
        bDescription
        (\() -> applyArguments bFunction)


ints1To100 : Array Int
ints1To100 =
    Array.fromList (List.range 1 100)
