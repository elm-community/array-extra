module Tests exposing (..)

import Array exposing (empty, fromList)
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
        ]
