module Candidates exposing (allRecursive, allWithFold, allWithListAll, anyRecursive, anyWithFold, anyWithListAny, filterMapWithListFilterMap, filterMapWithPush, indexedMapToListWithArrayIndexedMap, indexedMapToListWithFoldr, indexedMapToListWithListIndexedMap, indexedMapToListWithToIndexedList, intersperseWithArrayFoldr, intersperseWithList, map2WithListIndexedMap, map2WithListMap2, mapToListWithFoldr, mapToListWithListMap, memberRecursive, memberWithFold, memberWithList, negAbsComposeR, negAbsDeclarationArgumentPipeline, negAbsLambdaPipeline, reverseWithFoldl, reverseWithFoldlToList, reverseWithListReverse, sumNameOnlyCurried, sumNestedLambdaFullyAppliedCurried, sumOneLambdaFullyAppliedCurried, sumPartiallyCurried, sumPiping, unzipWithFoldlToArrays, unzipWithListUnzip, unzipWithMaps)

import Array exposing (Array)
import Array.Extra as Array



-- chain


negAbsDeclarationArgumentPipeline : number -> number
negAbsDeclarationArgumentPipeline n =
    n |> abs |> negate


negAbsLambdaPipeline : number -> number
negAbsLambdaPipeline =
    \n -> n |> abs |> negate


negAbsComposeR : number -> number
negAbsComposeR =
    abs >> negate



-- application


sumNameOnlyCurried : Array number -> number
sumNameOnlyCurried =
    Array.foldl (+) 0


sumPartiallyCurried : Array number -> number
sumPartiallyCurried =
    Array.foldl (\element -> (+) element) 0


sumPiping : Array number -> number
sumPiping =
    Array.foldl (\element soFar -> soFar |> (+) element) 0


sumOneLambdaFullyAppliedCurried : Array number -> number
sumOneLambdaFullyAppliedCurried =
    Array.foldl (\element soFar -> soFar + element) 0


sumNestedLambdaFullyAppliedCurried : Array number -> number
sumNestedLambdaFullyAppliedCurried =
    Array.foldl (\element -> \soFar -> soFar + element) 0



-- array extra


reverseWithFoldlToList : Array a -> Array a
reverseWithFoldlToList =
    Array.foldl (::) [] >> Array.fromList


reverseWithFoldl : Array a -> Array a
reverseWithFoldl =
    Array.foldl Array.push Array.empty


reverseWithListReverse : Array a -> Array a
reverseWithListReverse =
    Array.toList >> List.reverse >> Array.fromList


indexedMapToListWithFoldr : (Int -> a -> b) -> Array a -> List b
indexedMapToListWithFoldr mapIndexAndValue array =
    Array.foldr
        (\x ( i, ys ) ->
            ( i - 1, mapIndexAndValue i x :: ys )
        )
        ( Array.length array - 1, [] )
        array
        |> Tuple.second


indexedMapToListWithToIndexedList : (Int -> b -> a) -> Array b -> List a
indexedMapToListWithToIndexedList mapIndexAndValue =
    Array.toIndexedList
        >> List.map (\( i, v ) -> mapIndexAndValue i v)


indexedMapToListWithListIndexedMap : (Int -> a -> b) -> Array a -> List b
indexedMapToListWithListIndexedMap mapIndexAndValue =
    Array.toList >> List.indexedMap mapIndexAndValue


indexedMapToListWithArrayIndexedMap : (Int -> a -> b) -> Array a -> List b
indexedMapToListWithArrayIndexedMap mapIndexAndValue =
    Array.indexedMap mapIndexAndValue >> Array.toList


mapToListWithFoldr : (a -> b) -> Array a -> List b
mapToListWithFoldr alter =
    Array.foldr (alter >> (::)) []


mapToListWithListMap : (a -> b) -> Array a -> List b
mapToListWithListMap alter =
    Array.toList >> List.map alter


unzipWithMaps : Array ( a, b ) -> ( Array a, Array b )
unzipWithMaps tupleArray =
    ( tupleArray |> Array.map Tuple.first
    , tupleArray |> Array.map Tuple.second
    )


unzipWithListUnzip : Array ( a, b ) -> ( Array a, Array b )
unzipWithListUnzip tupleArray =
    tupleArray
        |> Array.toList
        |> List.unzip
        |> Tuple.mapBoth Array.fromList Array.fromList


unzipWithFoldlToArrays : Array ( a, b ) -> ( Array a, Array b )
unzipWithFoldlToArrays arrAB =
    Array.foldl
        (\( a, b ) ( arrA, arrB ) ->
            ( Array.push a arrA, Array.push b arrB )
        )
        ( Array.empty, Array.empty )
        arrAB


map2WithListMap2 : (a -> b -> c) -> Array a -> Array b -> Array c
map2WithListMap2 combine aArray bArray =
    List.map2 combine
        (Array.toList aArray)
        (Array.toList bArray)
        |> Array.fromList


map2WithListIndexedMap : (a -> b -> c) -> Array a -> Array b -> Array c
map2WithListIndexedMap combine aArray bArray =
    let
        length =
            min (Array.length aArray) (Array.length bArray)

        aList =
            Array.slice 0 length aArray
                |> Array.toList
    in
    aList
        |> List.indexedMap
            (\i a ->
                Maybe.map (combine a) (Array.get i bArray)
            )
        |> List.filterMap identity
        |> Array.fromList


filterMapWithPush : (a -> Maybe b) -> Array a -> Array b
filterMapWithPush f xs =
    let
        maybePush : (a -> Maybe b) -> a -> Array b -> Array b
        maybePush f_ mx xs_ =
            case f_ mx of
                Just x ->
                    Array.push x xs_

                Nothing ->
                    xs_
    in
    Array.foldl (maybePush f) Array.empty xs


filterMapWithListFilterMap : (a -> Maybe mapped) -> Array a -> Array mapped
filterMapWithListFilterMap tryMap =
    Array.toList
        >> List.filterMap tryMap
        >> Array.fromList


allRecursive : (a -> Bool) -> Array a -> Bool
allRecursive isOkay array =
    -- read & write is faster on the last element
    case Array.get (Array.length array - 1) array of
        Nothing ->
            True

        Just last ->
            if last |> isOkay then
                allRecursive isOkay (array |> Array.pop)

            else
                False


allWithListAll : (a -> Bool) -> Array a -> Bool
allWithListAll isOkay =
    Array.toList
        >> List.all isOkay


allWithFold : (a -> Bool) -> Array a -> Bool
allWithFold isOkay =
    Array.foldl
        (\element soFar -> soFar && isOkay element)
        True


anyWithListAny : (a -> Bool) -> Array a -> Bool
anyWithListAny isOkay =
    Array.toList
        >> List.any isOkay


anyWithFold : (a -> Bool) -> Array a -> Bool
anyWithFold isOkay =
    Array.foldl
        (\element soFar -> soFar || isOkay element)
        False


anyRecursive : (a -> Bool) -> Array a -> Bool
anyRecursive isOkay =
    \array ->
        -- read & write is faster on the last element
        case array |> Array.get ((array |> Array.length) - 1) of
            Nothing ->
                False

            Just last ->
                if last |> isOkay then
                    True

                else
                    array |> Array.pop |> anyRecursive isOkay


memberWithFold : a -> Array a -> Bool
memberWithFold needle =
    Array.foldl (\i res -> needle == i || res) False


memberRecursive : a -> Array a -> Bool
memberRecursive needle =
    memberRecursiveFromIndex 0 needle


memberRecursiveFromIndex : Int -> a -> Array a -> Bool
memberRecursiveFromIndex index needle =
    \array ->
        case array |> Array.get index of
            Just atIndex ->
                if atIndex == needle then
                    True

                else
                    array |> memberRecursiveFromIndex (index + 1) needle

            Nothing ->
                False


memberWithList : a -> Array a -> Bool
memberWithList needle =
    Array.toList >> List.member needle


intersperseWithArrayFoldr : a -> Array a -> Array a
intersperseWithArrayFoldr separator array =
    case Array.get (Array.length array - 1) array of
        Just last ->
            let
                beforeLast =
                    array |> Array.pop

                step element =
                    Array.push element
                        >> Array.push separator

                spersed =
                    beforeLast
                        |> Array.foldr step Array.empty
            in
            spersed |> Array.push last

        Nothing ->
            Array.empty


intersperseWithList : a -> Array a -> Array a
intersperseWithList separator array =
    array
        |> Array.toList
        |> List.intersperse separator
        |> Array.fromList
