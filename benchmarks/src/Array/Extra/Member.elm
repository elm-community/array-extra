module Array.Extra.Member exposing (recursive, recursiveFromIndex, withFold, withList)

import Array exposing (Array)


withFold : a -> Array a -> Bool
withFold needle =
    \array ->
        array
            |> Array.foldl (\i res -> needle == i || res) False


recursive : a -> Array a -> Bool
recursive needle =
    \array ->
        array |> recursiveFromIndex 0 needle


recursiveFromIndex : Int -> a -> Array a -> Bool
recursiveFromIndex index needle =
    \array ->
        case array |> Array.get index of
            Just atIndex ->
                if atIndex == needle then
                    True

                else
                    array |> recursiveFromIndex (index + 1) needle

            Nothing ->
                False


withList : a -> Array a -> Bool
withList needle =
    \array ->
        array |> Array.toList |> List.member needle
