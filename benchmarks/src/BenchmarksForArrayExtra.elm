module BenchmarksForArrayExtra exposing (main)

import Array exposing (Array)
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)


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
                directMapToList (\v -> -v)
                    num1To100
            )
            "mapToList with Array toIndexedList"
            (\() ->
                indirectMapToList (\v -> -v)
                    num1To100
            )
        , Benchmark.compare "indexedMapToList"
            "foldr"
            (\() ->
                directIndexedMapToList Tuple.pair
                    num1To100
            )
            "mapToList with Array toIndexedList"
            (\() ->
                indirectIndexedMapToList Tuple.pair
                    num1To100
            )
        ]


num1To100 : Array Int
num1To100 =
    Array.fromList (List.range 1 100)


directIndexedMapToList mapIndexAndValue array =
    Array.foldr
        (\x ( i, ys ) ->
            ( i - 1, mapIndexAndValue i x :: ys )
        )
        ( Array.length array - 1, [] )
        array
        |> Tuple.second


indirectIndexedMapToList mapIndexAndValue array =
    array
        |> Array.toIndexedList
        |> List.map (\( i, v ) -> mapIndexAndValue i v)


directMapToList alter =
    Array.foldl (alter >> (::)) []


indirectMapToList alter =
    Array.toList >> List.map alter
