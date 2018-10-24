# elm-json-codec

A library for creating combined encoders and decoders for Json in Elm, allowing composition.

With contributions from <a href='https://github.com/francescortiz'>francescortiz</a>

```
module Test exposing (..)

import Json.Decode as JD
import JsonCodec as JC
import Platform exposing (worker)

type alias Test = { i : Int, b : Bool, f : Float, o : Maybe String, s : String }
    
codec =
    Test
        |> JC.first  "i" JC.int .i
        |> JC.next   "b" JC.bool .b
        |> JC.next   "f" JC.float .f
        |> JC.option "o" (JC.nullable JC.string) .o Nothing
        |> JC.next   "s" JC.string .s
        |> JC.end

x =
    JD.decodeString (JC.decoder codec)
        "{\"i\":3,\"b\":false,\"f\":3.14,\"s\":\"hi there\"}"

y =
    JD.decodeString (JC.decoder codec)
        "{\"i\":3,\"b\":false,\"f\":3.14,\"o\":\"hi\",\"s\":\"hi there\"}"
```
