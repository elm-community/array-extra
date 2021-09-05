module Tests exposing (..)

import Array exposing (Array)
import Candidates exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "alternate functions"
        [ test "reverse(ToList) with Array.foldl"
            (\() ->
                reverseWithFoldlToList
                    (Array.fromList [ 1, 2, 3 ])
                    |> expectEqualArrays
                        (Array.fromList [ 3, 2, 1 ])
            )
        , test "mapToList with Array.foldr"
            (\() ->
                mapToListWithFoldr (\x -> -x)
                    (Array.fromList [ 1, 2, 3 ])
                    |> Expect.equal [ -1, -2, -3 ]
            )
        , describe "all"
            [ describe "recursive"
                [ test "True"
                    (\() ->
                        allRecursive identity
                            (Array.repeat 10 True)
                            |> Expect.equal True
                    )
                , test "False"
                    (\() ->
                        allRecursive identity
                            (Array.repeat 10 True
                                |> Array.push False
                            )
                            |> Expect.equal False
                    )
                ]
            , describe "with fold"
                [ test "True"
                    (\() ->
                        allWithFold identity
                            (Array.repeat 10 True)
                            |> Expect.equal True
                    )
                , test "False"
                    (\() ->
                        allWithFold identity
                            (Array.repeat 10 True
                                |> Array.push False
                            )
                            |> Expect.equal False
                    )
                ]
            ]
        , describe "any"
            [ describe "recursive"
                [ test "True"
                    (\() ->
                        anyRecursive isEven
                            ([ 1, 2 ] |> Array.fromList)
                            |> Expect.equal True
                    )
                , test "False"
                    (\() ->
                        anyRecursive isEven
                            ([ 1, 3 ] |> Array.fromList)
                            |> Expect.equal False
                    )
                ]
            , describe "with fold"
                [ test "True"
                    (\() ->
                        anyWithFold isEven
                            ([ 1, 2 ] |> Array.fromList)
                            |> Expect.equal True
                    )
                , test "False"
                    (\() ->
                        anyWithFold isEven
                            ([ 1, 3 ] |> Array.fromList)
                            |> Expect.equal False
                    )
                ]
            ]
        , test "intersperse with Array.foldr"
            (\() ->
                intersperseWithArrayFoldr "on"
                    (Array.fromList [ "turtles", "turtles", "turtles" ])
                    |> expectEqualArrays
                        (Array.fromList
                            [ "turtles", "on", "turtles", "on", "turtles" ]
                        )
            )
        ]


expectEqualArrays : Array a -> Array a -> Expectation
expectEqualArrays expected actual =
    Expect.equalLists
        (expected |> Array.toList)
        (actual |> Array.toList)


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0
