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

codec2 =
    Test
        |> JC.firstOpt  "i" JC.int .i -1
        |> JC.next   "b" JC.bool .b
        |> JC.next   "f" JC.float .f
        |> JC.option "o" (JC.nullable JC.string) .o Nothing
        |> JC.next   "s" JC.string .s
        |> JC.end

codec3 =
    JC.start Test
        |> JC.option  "i" JC.int .i -1
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
