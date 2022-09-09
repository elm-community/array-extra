module Array.Extra.Unzip exposing (withListUnzip, withMaps, wthFoldlToArrays)

import Array exposing (Array)


withMaps : Array ( a, b ) -> ( Array a, Array b )
withMaps =
    \arrayOfTuple ->
        ( arrayOfTuple |> Array.map Tuple.first
        , arrayOfTuple |> Array.map Tuple.second
        )


withListUnzip : Array ( a, b ) -> ( Array a, Array b )
withListUnzip =
    \arrayOfTuple ->
        arrayOfTuple
            |> Array.toList
            |> List.unzip
            |> Tuple.mapBoth Array.fromList Array.fromList


wthFoldlToArrays : Array ( a, b ) -> ( Array a, Array b )
wthFoldlToArrays =
    \arrayOfTuple ->
        arrayOfTuple
            |> Array.foldl
                (\( a, b ) ( arrayOfASoFar, arrayOfBSoFar ) ->
                    ( arrayOfASoFar |> Array.push a
                    , arrayOfBSoFar |> Array.push b
                    )
                )
                ( Array.empty, Array.empty )
