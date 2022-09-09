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
                    fromList [ 1, 2, 3 ]
                        |> update 1 ((+) 10)
                        |> expectEqualArrays
                            (fromList [ 1, 12, 3 ])
                )
            , test "negative index"
                (\() ->
                    fromList [ 1, 2, 3 ]
                        |> update -1 ((+) 10)
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            , test "too high index"
                (\() ->
                    fromList [ 1, 2, 3 ]
                        |> update 4 ((+) 10)
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            ]
        , describe "sliceFrom"
            [ test "valid positive index"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceFrom 3
                        |> expectEqualArrays
                            (fromList [ 3, 4, 5, 6 ])
                )
            , test "valid negative index"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceFrom -3
                        |> expectEqualArrays
                            (fromList [ 4, 5, 6 ])
                )
            , test "too high positive index"
                (\() ->
                    fromList (List.range 1 3)
                        |> sliceFrom 6
                        |> expectEqualArrays empty
                )
            , test "too low negative index"
                (\() ->
                    fromList (List.range 1 3)
                        |> sliceFrom -6
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            ]
        , describe "sliceUntil"
            [ test "valid positive index"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceUntil 3
                        |> expectEqualArrays
                            (fromList [ 0, 1, 2 ])
                )
            , test "valid negative index"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceUntil -3
                        |> expectEqualArrays
                            (fromList [ 0, 1, 2, 3 ])
                )
            , test "index 0"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceUntil 0
                        |> expectEqualArrays empty
                )
            , test "too high positive index"
                (\() ->
                    fromList (List.range 1 3)
                        |> sliceUntil 6
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            , test "too low negative index"
                (\() ->
                    fromList (List.range 1 3)
                        |> sliceUntil -6
                        |> expectEqualArrays empty
                )
            ]
        , describe "pop"
            [ test "empty"
                (\() ->
                    empty
                        |> pop
                        |> expectEqualArrays
                            empty
                )
            , test "filled"
                (\() ->
                    fromList [ 1, 2, 3 ]
                        |> pop
                        |> expectEqualArrays
                            (fromList [ 1, 2 ])
                )
            ]
        , test "filterMap"
            (\() ->
                fromList [ Just 3, Nothing, Just 5, Nothing ]
                    |> filterMap identity
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
        , Test.fuzz
            (Fuzz.array Fuzz.int)
            "mapToList"
            (\array ->
                array
                    |> mapToList String.fromInt
                    |> Expect.equalLists
                        (array
                            |> Array.map String.fromInt
                            |> Array.toList
                        )
            )
        , Test.fuzz
            (Fuzz.array Fuzz.string)
            "indexedMapToList"
            (\array ->
                array
                    |> indexedMapToList (\i el -> ( i, el ))
                    |> Expect.equalLists
                        (array
                            |> Array.toIndexedList
                        )
            )
        , describe "map2"
            -- `zip` will probably always be implemented with `map2`.
            -- No need to test both
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
        , -- `zip3` will probably always be implemented using `map3`.
          -- No need to test both
          describe "map3"
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
                num1234
                    |> removeWhen isEven
                    |> expectEqualArrays
                        (fromList [ 1, 3 ])
            )
        , test "unzip"
            (\() ->
                fromList [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
                    |> unzip
                    |> Expect.equal
                        ( fromList [ 1, 2, 3 ]
                        , fromList [ 'a', 'b', 'c' ]
                        )
            )
        , test "reverse"
            (\() ->
                num1234
                    |> reverse
                    |> expectEqualArrays
                        (fromList [ 4, 3, 2, 1 ])
            )
        , describe "resizelRepeat"
            [ test "length less than current"
                (\() ->
                    num1234
                        |> resizelRepeat 3 0
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            , test "length greater than current"
                (\() ->
                    num1234
                        |> resizelRepeat 6 0
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3, 4, 0, 0 ])
                )
            , test "negative length"
                (\() ->
                    num1234
                        |> resizelRepeat -1 0
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizerRepeat"
            [ test "length less than current"
                (\() ->
                    num1234
                        |> resizerRepeat 3 0
                        |> expectEqualArrays
                            (fromList [ 2, 3, 4 ])
                )
            , test "length greater than current"
                (\() ->
                    num1234
                        |> resizerRepeat 6 0
                        |> expectEqualArrays
                            (fromList [ 0, 0, 1, 2, 3, 4 ])
                )
            , test "negative length"
                (\() ->
                    num1234
                        |> resizelRepeat -1 0
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizelIndexed"
            [ test "length less than current"
                (\() ->
                    strAbc
                        |> resizelIndexed 2 String.fromInt
                        |> expectEqualArrays
                            (fromList [ "a", "b" ])
                )
            , test "length greater than current"
                (\() ->
                    strAbc
                        |> resizelIndexed 5 String.fromInt
                        |> expectEqualArrays
                            (fromList [ "a", "b", "c", "3", "4" ])
                )
            , test "negative length"
                (\() ->
                    strAbc
                        |> resizelIndexed -1 String.fromInt
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizerIndexed"
            [ test "length less than current"
                (\() ->
                    strAbc
                        |> resizerIndexed 2 String.fromInt
                        |> expectEqualArrays
                            (fromList [ "b", "c" ])
                )
            , test "length greater than current"
                (\() ->
                    strAbc
                        |> resizerIndexed 5 String.fromInt
                        |> expectEqualArrays
                            (fromList [ "0", "1", "a", "b", "c" ])
                )
            , test "negative length"
                (\() ->
                    strAbc
                        |> resizerIndexed -1 String.fromInt
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "splitAt"
            [ test "valid index"
                (\() ->
                    num1234
                        |> splitAt 2
                        |> Expect.equal
                            ( fromList [ 1, 2 ], fromList [ 3, 4 ] )
                )
            , test "negative index"
                (\() ->
                    num1234
                        |> splitAt -1
                        |> Expect.equal ( empty, num1234 )
                )
            , test "too high index"
                (\() ->
                    num1234
                        |> splitAt 100
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
            [ describe "True"
                [ Test.fuzz
                    (Fuzz.array Fuzz.int)
                    "filter test |> all test"
                    (\array ->
                        array
                            |> Array.filter isEven
                            |> all isEven
                            |> Expect.equal True
                    )
                , test "example"
                    (\() ->
                        fromList [ 2, 4 ]
                            |> all isEven
                            |> Expect.equal True
                    )
                ]
            , describe "False"
                [ Test.fuzz
                    (Fuzz.constant
                        (\before after -> { before = before, after = after })
                        |> Fuzz.andMap (Fuzz.array Fuzz.int)
                        |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    )
                    "1 failing element included"
                    (\{ before, after } ->
                        Array.append
                            (before |> Array.push 1)
                            after
                            |> all isEven
                            |> Expect.equal False
                    )
                , test "example"
                    (\() ->
                        fromList [ 2, 3 ]
                            |> all isEven
                            |> Expect.equal False
                    )
                ]
            ]
        , describe "any"
            [ describe "True"
                [ Test.fuzz
                    (Fuzz.constant
                        (\before after -> { before = before, after = after })
                        |> Fuzz.andMap (Fuzz.array Fuzz.int)
                        |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    )
                    "1 passing element included"
                    (\{ before, after } ->
                        Array.append
                            (before |> Array.push 2)
                            after
                            |> any isEven
                            |> Expect.equal True
                    )
                , test "example"
                    (\() ->
                        fromList [ 1, 2 ]
                            |> any isEven
                            |> Expect.equal True
                    )
                ]
            , describe "False"
                [ Test.fuzz
                    (Fuzz.array Fuzz.int)
                    "removeWhen test |> any test"
                    (\array ->
                        array
                            |> removeWhen isEven
                            |> any isEven
                            |> Expect.equal False
                    )
                , test "example"
                    (\() ->
                        fromList [ 1, 3 ]
                            |> any isEven
                            |> Expect.equal False
                    )
                ]
            ]
        , describe "intersperse"
            [ Test.fuzz Fuzz.int
                "empty"
                (\separator ->
                    empty
                        |> intersperse separator
                        |> expectEqualArrays empty
                )
            , Test.fuzz
                (Fuzz.constant
                    (\onlyElement separator ->
                        { onlyElement = onlyElement, separator = separator }
                    )
                    |> Fuzz.andMap Fuzz.int
                    |> Fuzz.andMap Fuzz.int
                )
                "one"
                (\{ onlyElement, separator } ->
                    Array.empty
                        |> Array.push onlyElement
                        |> intersperse separator
                        |> expectEqualArrays
                            (Array.empty
                                |> Array.push onlyElement
                            )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\base separator ->
                        { base = base, separator = separator }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap Fuzz.int
                )
                "combined length"
                (\{ base, separator } ->
                    base
                        |> intersperse separator
                        |> Array.length
                        |> Expect.equal
                            (((base |> Array.length) * 2 - 1)
                                |> max 0
                            )
                )
            , test "multiple"
                (\() ->
                    fromList [ "turtles", "turtles", "turtles" ]
                        |> intersperse "on"
                        |> expectEqualArrays
                            (fromList
                                [ "turtles", "on", "turtles", "on", "turtles" ]
                            )
                )
            ]
        , describe "interweave"
            [ Test.fuzz
                (Fuzz.constant
                    (\base toInterweave ->
                        { base = base, toInterweave = toInterweave }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                )
                "lengths add up"
                (\{ base, toInterweave } ->
                    base
                        |> interweave toInterweave
                        |> Array.length
                        |> Expect.equal
                            ((base |> Array.length)
                                + (toInterweave |> Array.length)
                            )
                )
            , test "less to interweave"
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
