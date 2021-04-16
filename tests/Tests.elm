module Tests exposing (..)

{-| Even though most implementations seem robust as they are now,
the tests are here to allow confident refactoring & changing.
-}

import Array exposing (empty, fromList, repeat)
import Array.Extra exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Array.Extra"
        [ test "update"
            (\() ->
                fromList [ 1, 2, 3 ]
                    |> Expect.all
                        [ update 1 ((+) 10)
                            >> Expect.equal (fromList [ 1, 12, 3 ])
                        , update 4 ((+) 10)
                            >> Expect.equal (fromList [ 1, 2, 3 ])
                        , update -1 ((+) 10)
                            >> Expect.equal (fromList [ 1, 2, 3 ])
                        ]
            )
        , test "sliceFrom"
            (\() ->
                fromList (List.range 0 9)
                    |> Expect.all
                        [ sliceFrom 5
                            >> Expect.equal (fromList [ 5, 6, 7, 8, 9 ])
                        , sliceFrom -3
                            >> Expect.equal (fromList [ 7, 8, 9 ])
                        ]
            )
        , test "sliceUntil"
            (\() ->
                fromList (List.range 0 9)
                    |> Expect.all
                        [ sliceUntil 5
                            >> Expect.equal (fromList [ 0, 1, 2, 3, 4 ])
                        , sliceUntil -3
                            >> Expect.equal (fromList [ 0, 1, 2, 3, 4, 5, 6 ])
                        ]
            )
        , test "pop"
            (\() ->
                Expect.all
                    [ \() ->
                        pop (fromList [ 1, 2, 3 ])
                            |> Expect.equal (fromList [ 1, 2 ])
                    , \() ->
                        pop empty
                            |> Expect.equal empty
                    ]
                    ()
            )
        , test "filterMap"
            (\() ->
                Expect.all
                    [ \() ->
                        filterMap String.toInt
                            (fromList [ "3", "4.0", "5", "hats" ])
                            |> Expect.equal (fromList [ 3, 5 ])
                    , \() ->
                        filterMap identity
                            (fromList [ Just 3, Nothing, Just 5, Nothing ])
                            |> Expect.equal (fromList [ 3, 5 ])
                    ]
                    ()
            )
        , test "apply"
            (\() ->
                apply
                    (fromList
                        [ \x -> -x
                        , identity
                        , (+) 10
                        , \x -> x - 10
                        , always 0
                        ]
                    )
                    |> Expect.all
                        [ \apply5 ->
                            apply5 (repeat 8 123)
                                |> Expect.equal
                                    (fromList
                                        [ -123
                                        , 123
                                        , 133
                                        , 113
                                        , 0
                                        ]
                                    )
                        , \apply5 ->
                            apply5 (repeat 2 123)
                                |> Expect.equal
                                    (fromList
                                        [ -123, 123 ]
                                    )
                        ]
            )
        , test "mapToList"
            (\() ->
                mapToList String.fromInt (fromList [ 1, 2, 3 ])
                    |> Expect.equal
                        [ "1", "2", "3" ]
            )
        , test "indexedMapToList"
            (\() ->
                fromList (List.range 0 3)
                    |> indexedMapToList (\i v -> ( i, v ))
                    |> Expect.equal
                        [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ]
            )
        , test "map2"
            (\() ->
                -- zip is implemented that way... no need to test
                map2 Tuple.pair
                    (fromList (List.range 0 3))
                    |> Expect.all
                        [ \zipWith0To3 ->
                            zipWith0To3 (fromList (List.range 3 8))
                                |> Expect.equal
                                    (fromList
                                        [ ( 0, 3 )
                                        , ( 1, 4 )
                                        , ( 2, 5 )
                                        , ( 3, 6 )
                                        ]
                                    )
                        , \zipWith0To3 ->
                            zipWith0To3 (fromList (List.range 3 5))
                                |> Expect.equal
                                    (fromList
                                        [ ( 0, 3 )
                                        , ( 1, 4 )
                                        , ( 2, 5 )
                                        ]
                                    )
                        ]
            )
        , test "map3"
            (\() ->
                -- zip3 is implemented that way... no need to test
                map3 (\a b c -> ( a, b, c ))
                    (fromList (List.range 0 3))
                    |> Expect.all
                        [ \zipWith0To3 ->
                            zipWith0To3
                                (fromList (List.range 10 13))
                                (fromList (List.range 3 8))
                                |> Expect.equal
                                    (fromList
                                        [ ( 0, 10, 3 )
                                        , ( 1, 11, 4 )
                                        , ( 2, 12, 5 )
                                        , ( 3, 13, 6 )
                                        ]
                                    )
                        , \zipWith0To3 ->
                            zipWith0To3
                                (fromList (List.range 10 13))
                                (fromList (List.range 3 5))
                                |> Expect.equal
                                    (fromList
                                        [ ( 0, 10, 3 )
                                        , ( 1, 11, 4 )
                                        , ( 2, 12, 5 )
                                        ]
                                    )
                        ]
            )
        , test "removeWhen"
            (\() ->
                removeWhen isEven (fromList [ 1, 2, 3, 4 ])
                    |> Expect.equal (fromList [ 1, 3 ])
            )
        , test "unzip"
            (\() ->
                unzip (fromList [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ])
                    |> Expect.equal
                        ( fromList [ 1, 2, 3 ]
                        , fromList [ 'a', 'b', 'c' ]
                        )
            )
        , test "reverse"
            (\() ->
                reverse (fromList [ 1, 2, 3 ])
                    |> Expect.equal (fromList [ 3, 2, 1 ])
            )

        --skipped resizer/l methods just for now
        --todo: understand them
        , test "splitAt"
            (\() ->
                fromList [ 1, 2, 3, 4 ]
                    |> Expect.all
                        [ splitAt 2
                            >> Expect.equal
                                ( fromList [ 1, 2 ], fromList [ 3, 4 ] )
                        , splitAt -1
                            >> Expect.equal
                                ( empty, fromList [ 1, 2, 3, 4 ] )
                        , splitAt 100
                            >> Expect.equal
                                ( fromList [ 1, 2, 3, 4 ], empty )
                        ]
            )
        , test "removeAt"
            (\() ->
                fromList [ 1, 2, 3, 4 ]
                    |> Expect.all
                        [ removeAt 2
                            >> Expect.equal
                                (fromList [ 1, 2, 4 ])
                        , removeAt -1
                            >> Expect.equal
                                (fromList [ 1, 2, 3, 4 ])
                        , removeAt 100
                            >> Expect.equal
                                (fromList [ 1, 2, 3, 4 ])
                        ]
            )
        ]


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0
