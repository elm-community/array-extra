module Array.Extra.All exposing (recursive, withFold, withListAll)

import Array exposing (Array)
import Array.Extra


recursive : (a -> Bool) -> Array a -> Bool
recursive isOkay =
    \array ->
        -- read & write is faster on the last element
        case array |> Array.get ((array |> Array.length) - 1) of
            Nothing ->
                True

            Just last ->
                if isOkay last then
                    recursive isOkay (Array.Extra.pop array)

                else
                    False


withListAll : (a -> Bool) -> Array a -> Bool
withListAll isOkay =
    \array ->
        array
            |> Array.toList
            |> List.all isOkay


withFold : (a -> Bool) -> Array a -> Bool
withFold isOkay =
    \array ->
        array
            |> Array.foldl
                (\element soFar -> soFar && isOkay element)
                True
