module Benchmarks exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Benchmark exposing (Benchmark, describe)
import Benchmark.Alternative exposing (rank)
import Benchmark.Runner.Alternative as BenchmarkRunner
import Candidates exposing (..)


main : BenchmarkRunner.Program
main =
    BenchmarkRunner.program suite


suite : Benchmark
suite =
    describe "array extra"
        [ rank "range from 0"
            (\f -> f ())
            [ ( "initialize"
              , \() -> Array.initialize 100 identity
              )
            , ( "from List.range 0 _"
              , \() -> Array.fromList (List.range 0 99)
              )
            ]
        , rank "List.any vs all"
            (\f -> f identity (List.repeat 100 True))
            [ ( "any", List.any )
            , ( "all", List.all )
            ]
        , rank "mapToList"
            (\mapToList -> mapToList (\v -> -v) ints1To100)
            [ ( "with foldr", mapToListWithFoldr )
            , ( "with Array.toIndexedList", mapToListWithListMap )
            ]
        , let
            compareToWithFoldr ( description, function ) =
                rank (description ++ " vs with Array.foldr")
                    (\indexedMapToList ->
                        indexedMapToList Tuple.pair
                            ints1To100
                    )
                    [ ( "with Array.foldr"
                      , indexedMapToListWithFoldr
                      )
                    , ( description, function )
                    ]
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
                rank
                    (description ++ " vs with Array.foldl to list")
                    (\reverse -> reverse ints1To100)
                    [ ( "with Array.foldl to list", reverseWithFoldlToList )
                    , ( description, function )
                    ]
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
                rank "vs with Array.maps"
                    (\unzip -> unzip zipped)
                    [ ( "with Array.maps", unzipWithMaps )
                    , ( description, function )
                    ]
          in
          describe "unzip"
            [ compareToWithArrayMaps
                ( "with List.unzip", unzipWithListUnzip )
            , compareToWithArrayMaps
                ( "with foldl to Arrays", unzipWithFoldlToArrays )
            ]
        , rank "map2"
            (\map2 ->
                map2 Tuple.pair
                    ints1To100
                    ints1To100
            )
            [ ( "with List.map2", map2WithListMap2 )
            , ( "with List.indexedMap", map2WithListIndexedMap )
            ]
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
          rank "filterMap"
            (\filterMap -> filterMap identity maybeInts)
            [ ( "with List.filterMap", filterMapWithListFilterMap )
            , ( "with push", filterMapWithPush )
            ]
        , let
            allTrue =
                Array.repeat 100 True
          in
          rank "all"
            (\all -> all identity allTrue)
            [ ( "with last and pop", allWithLastAndPop )
            , ( "with List.all", allWithListAll )
            , ( "with fold", allWithFold )
            ]
        , rank "intersperse"
            (\intersperse -> intersperse 0 ints1To100)
            [ ( "with Array.foldr", intersperseWithArrayFoldr )
            , ( "with List.intersperse", intersperseWithList )
            ]
        ]


ints1To100 : Array Int
ints1To100 =
    Array.fromList (List.range 1 100)
