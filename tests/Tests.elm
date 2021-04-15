module Tests exposing (..)

import Array exposing (fromList)
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
                        [ update 1 ((+) 10) >> Expect.equal (fromList [ 1, 12, 3 ])
                        , update 4 ((+) 10) >> Expect.equal (fromList [ 1, 2, 3 ])
                        , update -1 ((+) 10) >> Expect.equal (fromList [ 1, 2, 3 ])
                        ]
            )
        , test "sliceFrom"
            (\() ->
                fromList (List.range 0 9)
                    |> Expect.all
                        [ sliceFrom 5 >> Expect.equal (fromList [ 5, 6, 7, 8, 9 ])
                        , sliceFrom -3 >> Expect.equal (fromList [ 7, 8, 9 ])
                        ]
            )
        ]
