module Example exposing (..)

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
        ]
