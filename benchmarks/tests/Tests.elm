module Tests exposing (..)

import Array
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
                    |> Expect.equal (Array.fromList [ 3, 2, 1 ])
            )
        , test "mapToList with Array.foldr"
            (\() ->
                mapToListWithFoldr (\x -> -x)
                    (Array.fromList [ 1, 2, 3 ])
                    |> Expect.equal [ -1, -2, -3 ]
            )
        , describe "all with last and pop"
            [ test "True"
                (\() ->
                    allWithLastAndPop identity
                        (Array.repeat 10 True)
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    allWithLastAndPop identity
                        (Array.repeat 10 True
                            |> Array.push False
                        )
                        |> Expect.equal False
                )
            ]
        ]
