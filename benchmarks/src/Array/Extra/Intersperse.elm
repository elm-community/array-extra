module Array.Extra.Intersperse exposing (withArrayFoldr, withList)

import Array exposing (Array)
import Array.Extra


withArrayFoldr : a -> Array a -> Array a
withArrayFoldr separator =
    \array ->
        case array |> Array.get ((array |> Array.length) - 1) of
            Just last ->
                let
                    beforeLast =
                        array |> Array.Extra.pop

                    step element =
                        Array.push element
                            >> Array.push separator

                    beforeLastInterspersed =
                        beforeLast
                            |> Array.foldr step Array.empty
                in
                beforeLastInterspersed |> Array.push last

            Nothing ->
                Array.empty


withList : a -> Array a -> Array a
withList separator =
    \array ->
        array
            |> Array.toList
            |> List.intersperse separator
            |> Array.fromList
