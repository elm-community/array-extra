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
        [ rank "Array.fold"
            (\fold -> ints1To100 |> fold (+) 0)
            [ ( "foldl", Array.foldl )
            , ( "foldr", Array.foldr )
            ]
        , rank "mapToList"
            (\mapToList -> mapToList (\v -> -v) ints1To100)
            [ ( "with foldr", mapToListWithFoldr )
            , ( "with Array.toIndexedList", mapToListWithListMap )
            ]
        , rank "indexedMapToList"
            (\indexedMapToList ->
                indexedMapToList Tuple.pair
                    ints1To100
            )
            [ ( "with Array.foldr", indexedMapToListWithFoldr )
            , ( "with toIndexedList"
              , indexedMapToListWithToIndexedList
              )
            , ( "with Array.indexedMap"
              , indexedMapToListWithArrayIndexedMap
              )
            , ( "with List.indexedMap"
              , indexedMapToListWithListIndexedMap
              )
            ]
        , rank "reverse"
            (\reverse -> reverse ints1To100)
            [ ( "with Array.foldl to list", reverseWithFoldlToList )
            , ( "with List.reverse", reverseWithListReverse )
            , ( "with Array.foldr to array", reverseWithFoldl )
            ]
        , let
            zipped =
                Array.zip ints1To100 ints1To100
          in
          rank "unzip"
            (\unzip -> unzip zipped)
            [ ( "with Array.maps", unzipWithMaps )
            , ( "with List.unzip", unzipWithListUnzip )
            , ( "with foldl to Arrays", unzipWithFoldlToArrays )
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
            [ ( "recursive", allRecursive )
            , ( "with List.all", allWithListAll )
            , ( "with fold", allWithFold )
            ]
        , let
            allFalse =
                Array.repeat 100 False
          in
          rank "any"
            (\any -> any identity allFalse)
            [ ( "recursive", anyRecursive )
            , ( "with List.any", anyWithListAny )
            , ( "with fold", anyWithFold )
            ]
        , rank "intersperse"
            (\intersperse -> intersperse 0 ints1To100)
            [ ( "with Array.foldr", intersperseWithArrayFoldr )
            , ( "with List.intersperse", intersperseWithList )
            ]
        , let
            array =
                Array.fromList <| List.range 0 100
          in
          rank "member implementation"
            (\f -> f 50 array)
            [ ( "with foldr", arrayMemberFoldr )
            , ( "with foldl", arrayMemberFoldl )
            , ( "recursive", arrayMemberRec )
            , ( "with List.member", listMember )
            ]
        ]


ints1To100 : Array Int
ints1To100 =
    Array.fromList (List.range 1 100)
