module Tests exposing (..)

{-| Even though most implementations seem robust as they are now,
the tests are here to allow confident refactoring & changing.
-}

import Array exposing (Array, empty, fromList, repeat)
import Array.Extra exposing (..)
import Expect exposing (Expectation)
import Fuzz
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Array.Extra"
        [ describe "update"
            [ test "valid index"
                (\() ->
                    update 1 ((+) 10) (fromList [ 1, 2, 3 ])
                        |> expectEqualArrays
                            (fromList [ 1, 12, 3 ])
                )
            , test "negative index"
                (\() ->
                    update -1 ((+) 10) (fromList [ 1, 2, 3 ])
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            , test "too high index"
                (\() ->
                    update 4 ((+) 10) (fromList [ 1, 2, 3 ])
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            ]
        , describe "sliceFrom"
            [ test "valid positive index"
                (\() ->
                    sliceFrom 3 (fromList (List.range 0 6))
                        |> expectEqualArrays
                            (fromList [ 3, 4, 5, 6 ])
                )
            , test "valid negative index"
                (\() ->
                    sliceFrom -3 (fromList (List.range 0 6))
                        |> expectEqualArrays
                            (fromList [ 4, 5, 6 ])
                )
            , test "too high positive index"
                (\() ->
                    sliceFrom 6 (fromList (List.range 1 3))
                        |> expectEqualArrays empty
                )
            , test "too low negative index"
                (\() ->
                    sliceFrom -6 (fromList (List.range 1 3))
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            ]
        , describe "sliceUntil"
            [ test "valid positive index"
                (\() ->
                    sliceUntil 3 (fromList (List.range 0 6))
                        |> expectEqualArrays
                            (fromList [ 0, 1, 2 ])
                )
            , test "valid negative index"
                (\() ->
                    sliceUntil -3 (fromList (List.range 0 6))
                        |> expectEqualArrays
                            (fromList [ 0, 1, 2, 3 ])
                )
            , test "index 0"
                (\() ->
                    sliceUntil 0 (fromList (List.range 0 6))
                        |> expectEqualArrays empty
                )
            , test "too high positive index"
                (\() ->
                    sliceUntil 6 (fromList (List.range 1 3))
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            , test "too low negative index"
                (\() ->
                    sliceUntil -6 (fromList (List.range 1 3))
                        |> expectEqualArrays empty
                )
            ]
        , describe "pop"
            [ test "nonempty array"
                (\() ->
                    pop (fromList [ 1, 2, 3 ])
                        |> expectEqualArrays
                            (fromList [ 1, 2 ])
                )
            , test "empty"
                (\() ->
                    pop empty
                        |> expectEqualArrays empty
                )
            ]
        , test "filterMap"
            (\() ->
                filterMap identity
                    (fromList [ Just 3, Nothing, Just 5, Nothing ])
                    |> expectEqualArrays
                        (fromList [ 3, 5 ])
            )
        , describe "apply"
            (let
                fun4 =
                    fromList
                        [ \x -> -x
                        , identity
                        , (+) 10
                        , always 0
                        ]
             in
             [ test "more elements than functions"
                (\() ->
                    apply fun4 (repeat 5 100)
                        |> expectEqualArrays
                            (fromList
                                [ -100, 100, 110, 0 ]
                            )
                )
             , test "more functions than elements"
                (\() ->
                    apply fun4 (repeat 3 100)
                        |> expectEqualArrays
                            (fromList
                                [ -100, 100, 110 ]
                            )
                )
             ]
            )
        , test "mapToList"
            (\() ->
                mapToList String.fromInt num1234
                    |> Expect.equalLists
                        [ "1", "2", "3", "4" ]
            )
        , test "indexedMapToList"
            (\() ->
                indexedMapToList (\i v -> ( i, v )) strAbc
                    |> Expect.equalLists
                        [ ( 0, "a" ), ( 1, "b" ), ( 2, "c" ) ]
            )
        , describe "map2"
            -- zip will always be implemented with map2...
            -- no need for extra tests
            [ test "first array longer than the last"
                (\() ->
                    map2 Tuple.pair num1234 chrAbcde
                        |> expectEqualArrays
                            (fromList
                                [ ( 1, 'a' )
                                , ( 2, 'b' )
                                , ( 3, 'c' )
                                , ( 4, 'd' )
                                ]
                            )
                )
            , test "first array shorter than the last"
                (\() ->
                    map2 Tuple.pair chrAbcde num1234
                        |> expectEqualArrays
                            (fromList
                                [ ( 'a', 1 )
                                , ( 'b', 2 )
                                , ( 'c', 3 )
                                , ( 'd', 4 )
                                ]
                            )
                )
            ]
        , describe "map3"
            -- zip3 will probably always be implemented using map3...
            -- no need for extra tests
            [ test "first array the shortest"
                (\() ->
                    map3 (\a b c -> ( a, b, c )) strAbc chrAbcde num1234
                        |> expectEqualArrays
                            (fromList
                                [ ( "a", 'a', 1 )
                                , ( "b", 'b', 2 )
                                , ( "c", 'c', 3 )
                                ]
                            )
                )
            , test "second array the shortest"
                (\() ->
                    map3 (\a b c -> ( a, b, c )) chrAbcde strAbc num1234
                        |> expectEqualArrays
                            (fromList
                                [ ( 'a', "a", 1 )
                                , ( 'b', "b", 2 )
                                , ( 'c', "c", 3 )
                                ]
                            )
                )
            , test "third array the shortest"
                (\() ->
                    map3 (\a b c -> ( a, b, c )) chrAbcde num1234 strAbc
                        |> expectEqualArrays
                            (fromList
                                [ ( 'a', 1, "a" )
                                , ( 'b', 2, "b" )
                                , ( 'c', 3, "c" )
                                ]
                            )
                )
            ]
        , test "removeWhen"
            (\() ->
                removeWhen isEven num1234
                    |> expectEqualArrays (fromList [ 1, 3 ])
            )
        , test "unzip"
            (\() ->
                unzip
                    (fromList
                        [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
                    )
                    |> Expect.equal
                        ( fromList [ 1, 2, 3 ]
                        , fromList [ 'a', 'b', 'c' ]
                        )
            )
        , test "reverse"
            (\() ->
                reverse num1234
                    |> expectEqualArrays
                        (fromList [ 4, 3, 2, 1 ])
            )
        , describe "resizelRepeat"
            [ test "length less than current"
                (\() ->
                    resizelRepeat 3 0 num1234
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            , test "length greater than current"
                (\() ->
                    resizelRepeat 6 0 num1234
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3, 4, 0, 0 ])
                )
            , test "negative length"
                (\() ->
                    resizelRepeat -1 0 num1234
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizerRepeat"
            [ test "length less than current"
                (\() ->
                    resizerRepeat 3 0 num1234
                        |> expectEqualArrays
                            (fromList [ 2, 3, 4 ])
                )
            , test "length greater than current"
                (\() ->
                    resizerRepeat 6 0 num1234
                        |> expectEqualArrays
                            (fromList [ 0, 0, 1, 2, 3, 4 ])
                )
            , test "negative length"
                (\() ->
                    resizelRepeat -1 0 num1234
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizelIndexed"
            [ test "length less than current"
                (\() ->
                    resizelIndexed 2 String.fromInt strAbc
                        |> expectEqualArrays
                            (fromList [ "a", "b" ])
                )
            , test "length greater than current"
                (\() ->
                    resizelIndexed 5 String.fromInt strAbc
                        |> expectEqualArrays
                            (fromList [ "a", "b", "c", "3", "4" ])
                )
            , test "negative length"
                (\() ->
                    resizelIndexed -1 String.fromInt strAbc
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizerIndexed"
            [ test "length less than current"
                (\() ->
                    resizerIndexed 2 String.fromInt strAbc
                        |> expectEqualArrays
                            (fromList [ "b", "c" ])
                )
            , test "length greater than current"
                (\() ->
                    resizerIndexed 5 String.fromInt strAbc
                        |> expectEqualArrays
                            (fromList [ "0", "1", "a", "b", "c" ])
                )
            , test "negative length"
                (\() ->
                    resizerIndexed -1 String.fromInt strAbc
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "splitAt"
            [ test "valid index"
                (\() ->
                    splitAt 2 num1234
                        |> Expect.equal
                            ( fromList [ 1, 2 ], fromList [ 3, 4 ] )
                )
            , test "negative index"
                (\() ->
                    splitAt -1 num1234
                        |> Expect.equal ( empty, num1234 )
                )
            , test "too high index"
                (\() ->
                    splitAt 100 num1234
                        |> Expect.equal ( num1234, empty )
                )
            ]
        , describe "removeAt"
            [ test "valid index"
                (\() ->
                    removeAt 2 num1234
                        |> expectEqualArrays
                            (fromList [ 1, 2, 4 ])
                )
            , test "negative index"
                (\() ->
                    removeAt -1 num1234
                        |> expectEqualArrays num1234
                )
            , test "too high index"
                (\() ->
                    removeAt 100 num1234
                        |> expectEqualArrays num1234
                )
            ]
        , let
            ac =
                fromList [ 'a', 'c' ]
          in
          describe "insertAt"
            [ test "valid index"
                (\() ->
                    insertAt 1 'b' ac
                        |> expectEqualArrays
                            (fromList [ 'a', 'b', 'c' ])
                )
            , test "negative index"
                (\() ->
                    insertAt -1 'b' ac
                        |> expectEqualArrays ac
                )
            , test "too high index"
                (\() ->
                    insertAt 100 'b' ac
                        |> expectEqualArrays ac
                )
            ]
        , describe "all"
            [ test "True"
                (\() ->
                    all isEven (fromList [ 2, 4 ])
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    all isEven (fromList [ 2, 3 ])
                        |> Expect.equal False
                )
            ]
        , describe "any"
            [ test "True"
                (\() ->
                    any isEven (fromList [ 1, 2 ])
                        |> Expect.equal True
                )
            , test "False"
                (\() ->
                    any isEven (fromList [ 1, 3 ])
                        |> Expect.equal False
                )
            ]
        , test "intersperse"
            (\() ->
                intersperse "on"
                    (fromList [ "turtles", "turtles", "turtles" ])
                    |> expectEqualArrays
                        (fromList
                            [ "turtles", "on", "turtles", "on", "turtles" ]
                        )
            )
        , describe "interweave"
            [ test "less to interweave"
                (\() ->
                    fromList [ "a0", "a1", "a2" ]
                        |> interweave
                            (fromList [ "b0" ])
                        |> expectEqualArrays
                            (fromList
                                [ "a0", "b0", "a1", "a2" ]
                            )
                )
            , test "more to interweave"
                (\() ->
                    fromList [ "a0", "a1", "a2" ]
                        |> interweave
                            (fromList [ "b0", "b1", "b2", "b3", "b4" ])
                        |> expectEqualArrays
                            (fromList
                                [ "a0", "b0", "a1", "b1", "a2", "b2", "b3", "b4" ]
                            )
                )
            ]
        , describe "member"
            [ Test.fuzz
                (Fuzz.constant
                    (\before after ->
                        { before = before, after = after }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                )
                "exists"
                (\{ before, after } ->
                    Array.append
                        (before |> Array.push 123456)
                        after
                        |> member 123456
                        |> Expect.equal True
                )
            , Test.fuzz
                (Fuzz.array Fuzz.int)
                "doesn't exist"
                (Array.filter (\element -> element /= 123456)
                    >> member 123456
                    >> Expect.equal False
                )
            ]
        ]



-- used


num1234 : Array number
num1234 =
    fromList [ 1, 2, 3, 4 ]


{-| Lowercase letters 'a' to 'e'.
-}
chrAbcde : Array Char
chrAbcde =
    fromList [ 'a', 'b', 'c', 'd', 'e' ]


{-| Lowercase character strings "a", "b", "c".
-}
strAbc : Array String
strAbc =
    fromList [ "a", "b", "c" ]


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0


expectEqualArrays : Array a -> Array a -> Expectation
expectEqualArrays expected actual =
    Expect.equalLists
        (expected |> Array.toList)
        (actual |> Array.toList)
