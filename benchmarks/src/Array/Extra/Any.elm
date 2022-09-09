module Array.Extra.Any exposing (recursive, withFold, withList)

import Array exposing (Array)
import Array.Extra


withList : (a -> Bool) -> Array a -> Bool
withList isOkay =
    \array ->
        array
            |> Array.toList
            |> List.any isOkay


withFold : (a -> Bool) -> Array a -> Bool
withFold isOkay =
    \array ->
        array
            |> Array.foldl
                (\element soFar -> soFar || isOkay element)
                False


recursive : (a -> Bool) -> Array a -> Bool
recursive isOkay =
    \array ->
        -- read & write is faster on the last element
        case array |> Array.get ((array |> Array.length) - 1) of
            Nothing ->
                False

            Just last ->
                if last |> isOkay then
                    True

                else
                    array |> Array.Extra.pop |> recursive isOkay
