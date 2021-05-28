module Benchmarks exposing (main)

import Array exposing (Array)
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import Candidates exposing (..)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    describe "array extra"
        [ Benchmark.compare "from 0 to 99"
            "initialize"
            (\() ->
                Array.initialize 100 (\i -> i)
            )
            "fromList (List.range 0 _)"
            (\() ->
                Array.fromList (List.range 0 99)
            )
        , Benchmark.compare "mapToList"
            "foldr"
            (\() ->
                mapToListWithFoldr (\v -> -v)
                    num1To100
            )
            "mapToList with Array toIndexedList"
            (\() ->
                mapToListWithListMap (\v -> -v)
                    num1To100
            )
        , let
            compareToWithFoldr description function =
                Benchmark.compare (description ++ " vs with Array.foldr")
                    "with Array.foldr"
                    (\() ->
                        indexedMapToListWithFoldr Tuple.pair
                            num1To100
                    )
                    description
                    (\() ->
                        function Tuple.pair
                            num1To100
                    )
          in
          describe "indexedMapToList"
            [ compareToWithFoldr "with toIndexedList"
                indexedMapToListWithToIndexedList
            , compareToWithFoldr "with Array.indexedMap"
                indexedMapToListWithArrayIndexedMap
            , compareToWithFoldr "with List.indexedMap"
                indexedMapToListWithListIndexedMap
            ]
        , let
            compareToFoldlToList description function =
                Benchmark.compare
                    (description ++ " vs with Array.foldl to list")
                    "with Array.foldl to list"
                    (\() ->
                        reverseWithFoldlToList num1To100
                    )
                    description
                    (\() -> function num1To100)
          in
          describe "reverse"
            [ compareToFoldlToList
                "with List.reverse"
                reverseWithListReverse
            , compareToFoldlToList
                "with Array.foldr to array"
                reverseWithFoldl
            ]
        ]


num1To100 : Array Int
num1To100 =
    Array.fromList (List.range 1 100)
