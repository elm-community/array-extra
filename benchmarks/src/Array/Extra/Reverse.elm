module Array.Extra.Reverse exposing (withFoldl, withFoldlToList, withListReverse)

import Array exposing (Array)


withFoldlToList : Array a -> Array a
withFoldlToList =
    \array ->
        array |> Array.foldl (::) [] |> Array.fromList


withFoldl : Array a -> Array a
withFoldl =
    \array ->
        array |> Array.foldl Array.push Array.empty


withListReverse : Array a -> Array a
withListReverse =
    \array ->
        array |> Array.toList |> List.reverse |> Array.fromList
