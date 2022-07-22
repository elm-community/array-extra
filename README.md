# convenience functions for working with `Array`

Some people like to import under the same namespace as `Array`:

```elm
import Array exposing (Array)
import Array.Extra as Array

firstFive : Array a -> Array a
firstFive =
    Array.sliceUntil 5
```

Note that this API is experimental and likely to go through many more iterations.

Feedback and contributions are very welcome.
