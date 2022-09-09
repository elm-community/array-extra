module Array.Extra.Map2 exposing (withListIndexedMap, withListMap2)

import Array exposing (Array)


withListMap2 : (a -> b -> c) -> Array a -> Array b -> Array c
withListMap2 combine aArray bArray =
    List.map2 combine
        (Array.toList aArray)
        (Array.toList bArray)
        |> Array.fromList


withListIndexedMap : (a -> b -> c) -> Array a -> Array b -> Array c
withListIndexedMap combine aArray bArray =
    let
        length =
            min
                (aArray |> Array.length)
                (bArray |> Array.length)

        aList =
            aArray
                |> Array.slice 0 length
                |> Array.toList
    in
    aList
        |> List.indexedMap
            (\i a ->
                Maybe.map (combine a) (bArray |> Array.get i)
            )
        |> List.filterMap identity
        |> Array.fromList
