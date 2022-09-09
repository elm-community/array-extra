module Tests exposing (suite)

{-| Even though most implementations seem robust as they are now,
the tests are here to allow confident refactoring & changing.
-}

import Array exposing (Array, empty, fromList, repeat)
import Array.Extra exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Array.Extra"
        [ describe "all"
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
                (\array ->
                    array
                        |> Array.filter (\element -> element /= 123456)
                        |> member 123456
                        |> Expect.equal False
                )
            ]
        , describe "update"
            [ test "index valid"
                (\() ->
                    fromList [ 1, 2, 3 ]
                        |> update 1 (\n -> n + 10)
                        |> expectEqualArrays
                            (fromList [ 1, 12, 3 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative"
                (\{ array, index } ->
                    array
                        |> update index (\n -> n + 10)
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index too high"
                (\{ array, above } ->
                    array
                        |> update ((array |> Array.length) + above) (\n -> n + 10)
                        |> expectEqualArrays
                            array
                )
            ]
        , describe "pop"
            [ test "empty → empty"
                (\() ->
                    empty
                        |> pop
                        |> expectEqualArrays
                            empty
                )
            , Test.fuzz
                (Fuzz.constant
                    (\beforeLast last ->
                        { beforeLast = beforeLast, last = last }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap Fuzz.int
                )
                "push |> pop  → no change"
                (\{ beforeLast, last } ->
                    beforeLast
                        |> Array.push last
                        |> pop
                        |> expectEqualArrays
                            beforeLast
                )
            ]
        , describe "splitAt"
            [ test "index valid"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> splitAt 2
                        |> Expect.equal
                            ( fromList [ 1, 2 ], fromList [ 3, 4 ] )
                )
            , test "index negative"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> splitAt -1
                        |> Expect.equal ( empty, fromList [ 1, 2, 3, 4 ] )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index too high"
                (\{ array, above } ->
                    array
                        |> splitAt ((array |> Array.length) + above)
                        |> Expect.equal ( array, empty )
                )
            ]
        , describe "removeAt"
            [ test "index valid"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> removeAt 2
                        |> expectEqualArrays
                            (fromList [ 1, 2, 4 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative"
                (\{ array, index } ->
                    array
                        |> removeAt index
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index too high"
                (\{ array, above } ->
                    array
                        |> removeAt ((array |> Array.length) + above)
                        |> expectEqualArrays
                            array
                )
            ]
        , describe "insertAt"
            [ test "index valid"
                (\() ->
                    fromList [ 'a', 'c' ]
                        |> insertAt 1 'b'
                        |> expectEqualArrays
                            (fromList [ 'a', 'b', 'c' ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative"
                (\{ array, index } ->
                    array
                        |> insertAt index 12345
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 1 Random.maxInt)
                )
                "index too high"
                (\{ array, above } ->
                    array
                        |> insertAt ((array |> Array.length) + above) 12345
                        |> expectEqualArrays
                            array
                )
            ]
        , describe "sliceFrom"
            [ test "index positive valid"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceFrom 3
                        |> expectEqualArrays
                            (fromList [ 3, 4, 5, 6 ])
                )
            , test "index negative valid"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceFrom -3
                        |> expectEqualArrays
                            (fromList [ 4, 5, 6 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index positive too high"
                (\{ array, above } ->
                    array
                        |> sliceFrom ((array |> Array.length) + above)
                        |> expectEqualArrays
                            empty
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array below -> { array = array, below = below })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index negative too low"
                (\{ array, below } ->
                    array
                        |> sliceFrom (-(array |> Array.length) - below)
                        |> expectEqualArrays
                            array
                )
            ]
        , describe "sliceUntil"
            [ test "index positive valid"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceUntil 3
                        |> expectEqualArrays
                            (fromList [ 0, 1, 2 ])
                )
            , test "index negative valid"
                (\() ->
                    fromList (List.range 0 6)
                        |> sliceUntil -3
                        |> expectEqualArrays
                            (fromList [ 0, 1, 2, 3 ])
                )
            , Test.fuzz
                (Fuzz.array Fuzz.int)
                "index 0"
                (\array ->
                    array
                        |> sliceUntil 0
                        |> expectEqualArrays
                            empty
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index positive too high"
                (\{ array, above } ->
                    array
                        |> sliceUntil ((array |> Array.length) + above)
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array below -> { array = array, below = below })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index negative too low"
                (\{ array, below } ->
                    array
                        |> sliceUntil (-(array |> Array.length) - below)
                        |> expectEqualArrays
                            empty
                )
            ]
        , describe "filterMap"
            [ Test.fuzz
                (Fuzz.array Fuzz.int)
                "all Just"
                (\array ->
                    array
                        |> Array.map Just
                        |> filterMap identity
                        |> expectEqualArrays array
                )
            , Test.fuzz
                (Fuzz.array (Fuzz.constant Nothing))
                "all Nothing"
                (\arrayOfNothing ->
                    arrayOfNothing
                        |> filterMap identity
                        |> expectEqualArrays empty
                )
            , test "some Nothing"
                (\() ->
                    fromList [ Just 3, Nothing, Just 5, Nothing ]
                        |> filterMap identity
                        |> expectEqualArrays
                            (fromList [ 3, 5 ])
                )
            ]
        , describe "apply"
            [ test "more elements than functions"
                (\() ->
                    repeat 5 100
                        |> apply
                            (fromList
                                [ negate
                                , identity
                                , \n -> n + 10
                                , \_ -> 0
                                ]
                            )
                        |> expectEqualArrays
                            (fromList
                                [ -100, 100, 110, 0 ]
                            )
                )
            , test "more functions than elements"
                (\() ->
                    repeat 3 100
                        |> apply
                            (fromList
                                [ negate
                                , identity
                                , \n -> n + 10
                                , \_ -> 0
                                ]
                            )
                        |> expectEqualArrays
                            (fromList
                                [ -100, 100, 110 ]
                            )
                )
            ]
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
                    map2 Tuple.pair
                        (fromList [ 1, 2, 3, 4 ])
                        (fromList [ 'a', 'b', 'c', 'd', 'e' ])
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
                    map2 Tuple.pair
                        (fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        (fromList [ 1, 2, 3, 4 ])
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
                    map3 (\a b c -> ( a, b, c ))
                        (fromList [ "a", "b", "c" ])
                        (fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        (fromList [ 1, 2, 3, 4 ])
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
                    map3 (\a b c -> ( a, b, c ))
                        (fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        (fromList [ "a", "b", "c" ])
                        (fromList [ 1, 2, 3, 4 ])
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
                    map3 (\a b c -> ( a, b, c ))
                        (fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        (fromList [ 1, 2, 3, 4 ])
                        (fromList [ "a", "b", "c" ])
                        |> expectEqualArrays
                            (fromList
                                [ ( 'a', 1, "a" )
                                , ( 'b', 2, "b" )
                                , ( 'c', 3, "c" )
                                ]
                            )
                )
            ]
        , describe "removeWhen"
            [ test "example"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> removeWhen isEven
                        |> expectEqualArrays
                            (fromList [ 1, 3 ])
                )
            , Test.fuzz
                (Fuzz.array Fuzz.int)
                "filter is |> removeWhen is  → empty"
                (\array ->
                    array
                        |> Array.filter isEven
                        |> removeWhen isEven
                        |> expectEqualArrays
                            empty
                )
            , Test.fuzz
                (Fuzz.array Fuzz.int)
                "removeWhen is |> filter is  → empty"
                (\array ->
                    array
                        |> removeWhen isEven
                        |> Array.filter isEven
                        |> expectEqualArrays
                            empty
                )
            ]
        , test "unzip"
            (\() ->
                fromList [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
                    |> unzip
                    |> Expect.equal
                        ( fromList [ 1, 2, 3 ]
                        , fromList [ 'a', 'b', 'c' ]
                        )
            )
        , describe "reverse"
            [ Test.fuzz
                (Fuzz.list Fuzz.int)
                "like List.reverse"
                (\list ->
                    list
                        |> Array.fromList
                        |> reverse
                        |> expectEqualArrays
                            (list |> List.reverse |> Array.fromList)
                )
            , test "example"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> reverse
                        |> expectEqualArrays
                            (fromList [ 4, 3, 2, 1 ])
                )
            ]
        , describe "resizelRepeat"
            [ test "length less than current"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> resizelRepeat 3 0
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3 ])
                )
            , test "length greater than current"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> resizelRepeat 6 0
                        |> expectEqualArrays
                            (fromList [ 1, 2, 3, 4, 0, 0 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array length -> { array = array, length = length })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt 0)
                )
                "non-positive length  → empty"
                (\{ array, length } ->
                    array
                        |> resizelRepeat length 0
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizerRepeat"
            [ test "length less than current"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> resizerRepeat 3 0
                        |> expectEqualArrays
                            (fromList [ 2, 3, 4 ])
                )
            , test "length greater than current"
                (\() ->
                    fromList [ 1, 2, 3, 4 ]
                        |> resizerRepeat 6 0
                        |> expectEqualArrays
                            (fromList [ 0, 0, 1, 2, 3, 4 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array length -> { array = array, length = length })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt 0)
                )
                "non-positive length  → empty"
                (\{ array, length } ->
                    array
                        |> resizelRepeat length 0
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizelIndexed"
            [ test "length less than current"
                (\() ->
                    fromList [ "a", "b", "c" ]
                        |> resizelIndexed 2 String.fromInt
                        |> expectEqualArrays
                            (fromList [ "a", "b" ])
                )
            , test "length greater than current"
                (\() ->
                    fromList [ "a", "b", "c" ]
                        |> resizelIndexed 5 String.fromInt
                        |> expectEqualArrays
                            (fromList [ "a", "b", "c", "3", "4" ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "negative length  → empty"
                (\{ array, index } ->
                    fromList [ "a", "b", "c" ]
                        |> resizelIndexed -1 String.fromInt
                        |> expectEqualArrays Array.empty
                )
            ]
        , describe "resizerIndexed"
            [ test "length less than current"
                (\() ->
                    fromList [ "a", "b", "c" ]
                        |> resizerIndexed 2 String.fromInt
                        |> expectEqualArrays
                            (fromList [ "b", "c" ])
                )
            , test "length greater than current"
                (\() ->
                    fromList [ "a", "b", "c" ]
                        |> resizerIndexed 5 String.fromInt
                        |> expectEqualArrays
                            (fromList [ "0", "1", "a", "b", "c" ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.string)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "negative length"
                (\{ array, index } ->
                    array
                        |> resizerIndexed index String.fromInt
                        |> expectEqualArrays Array.empty
                )
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
        ]


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0


expectEqualArrays : Array a -> Array a -> Expectation
expectEqualArrays expected actual =
    Expect.equalLists
        (expected |> Array.toList)
        (actual |> Array.toList)
