module Array.Extra.FilterMap exposing (withListFilterMap, withPush)

import Array exposing (Array)


withPush : (a -> Maybe b) -> Array a -> Array b
withPush elementParse =
    \array ->
        Array.foldl
            (\element soFar ->
                soFar |> maybePush (element |> elementParse)
            )
            Array.empty
            array


maybePush : Maybe a -> Array a -> Array a
maybePush maybe =
    case maybe of
        Just value ->
            \array -> array |> Array.push value

        Nothing ->
            identity


withListFilterMap : (a -> Maybe mapped) -> Array a -> Array mapped
withListFilterMap tryMap =
    \array ->
        array
            |> Array.toList
            |> List.filterMap tryMap
            |> Array.fromList
