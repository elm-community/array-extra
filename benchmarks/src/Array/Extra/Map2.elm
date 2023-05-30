module Array.Extra.Map2 exposing (withIndexedMap, withListMap2)

import Array exposing (Array)
import Array.Extra


withListMap2 : (a -> b -> c) -> Array a -> Array b -> Array c
withListMap2 combine aArray bArray =
    List.map2 combine
        (Array.toList aArray)
        (Array.toList bArray)
        |> Array.fromList


withIndexedMap : (a -> b -> c) -> Array a -> Array b -> Array c
withIndexedMap combine aArray bArray =
    let
        length =
            min
                (aArray |> Array.length)
                (bArray |> Array.length)
    in
    aArray
        |> Array.slice 0 length
        |> Array.indexedMap
            (\i a ->
                Maybe.map (combine a) (bArray |> Array.get i)
            )
        |> Array.Extra.filterMap identity
