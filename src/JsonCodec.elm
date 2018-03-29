module JsonCodec exposing
    ( Codec
    , Builder
    , string
    , bool
    , int
    , float
    , nullable
    , list
    , array
    , dict
    , keyValuePairs
    , singleton
    , object2
    , object3
    , object4
    , object5
    , object6
    , object7
    , object8
    , map
    , oneOf
    , null
    , succeed
    , fail
    , andThen
    , decoder
    , encoder
    , init
    , first
    , next
    , end
    )

{-| Build json decoders and encoders simultaneously, conserving the need for
functions that do each in simple cases.  This allows a single datum to embody
both the encoder and the decoder for a type, reducing some duplication and
redundancy.  In simple cases, a codec can be built exclusively by composing
other codecs.

In more complex cases, ```fold``` could be used, which allows fields to be
encoded and extracted separately.

To just specify a decoder and encoder separately, use ```init```.

    module Session exposing (..)

    import JsonCodec exposing (Codec)
    import Dict exposing (Dict)
    import Json.Decode as JD
    import Json.Encode as JE

    type alias Session =
        { queue : List String
        , playing : Maybe (String, Float)
        , likeCategories : Dict String Int
        }

    type Alternative = A | Unknown String

    -- An example showing the use of ```map``` to augment a
    -- bijective encoding.
    altcoder : Codec Alternative
    altcoder =
        JsonCodec.string
        |> JsonCodec.map
            (\a -> if a == "A" then A else Unknown a)
            (\v ->
                case v of
                    A -> "A"
                    Unknown x -> x
            )

    type Picky = OptA | OptB
    pickyOpts : Dict String Picky
    pickyOpts = Dict.fromList [("A", OptA), ("B", OptB)]

    -- An example showing the use of ```andThen``` to exclude
    -- bad values.
    pickycoder : Codec Picky
    pickycoder =
        JsonCodec.string |> JsonCodec.andThen
            (((flip Dict.get) pickyOpts)
            >> Maybe.map JD.succeed
            >> Maybe.withDefault (JD.fail "Option must be A or B")
            )
            (\p -> pickyOpts
            |> Dict.toList
            |> List.filterMap (\(k,v) -> if v == p then Just k else Nothing)
            |> List.head
            |> Maybe.withDefault "A"
            )

    -- A codec that can stand on its own and also be reused.
    playingSerializer : Codec (Maybe (String,Float))
    playingSerializer =
        JsonCodec.nullable
            (JsonCodec.object2
                (,)
                identity
                ("u", JsonCodec.string)
                ("p", JsonCodec.float)
            )

    -- Simple codec built with composition.
    serializer : Codec Session
    serializer =
        JsonCodec.object3
            Session
            (\s -> (s.queue,s.playing,s.likeCategories))
            ("queue", JsonCodec.list JsonCodec.string)
            ("playing", playingSerializer)
            ("like", JsonCodec.dict JsonCodec.int)

    -- Codec built in applicative style
    type alias Test = { i : Int, b : Bool, f : Float, s : String }

    codec = 
        Test
        |> JC.first "i" JC.int .i
        |> JC.next "b" JC.bool .b
        |> JC.next "f" JC.float .f
        |> JC.next "s" JC.string .s
        |> JC.end

    x = JD.decodeString (JC.decoder be) "{\"i\":3,\"b\":false,\"f\":3.14,\"s\":\"hi there\"}"
    -- Ok { i = 3, b = False, f = 3.14, s = "hi there" }

# Type
@docs Codec, Builder

# Simple codecs
@docs string, bool, int, float, nullable, list, array, dict, keyValuePairs, singleton, object2, object3, object4, object5, object6, object7, object8, null, succeed, fail

# Transform in both directions
@docs map, andThen

# Decoding alternatives
@docs oneOf

# Construction
@docs decoder, encoder, init

# Application
@docs first, next, end

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Result.Extra

{-| The type of codecs constructed by the library.

You can extract a Json.Decode.Decoder with ```decoder``` and a function that
constructs ```Json.Encoder.Value``` with ```encoder```.
-}
type alias Codec a = (JD.Decoder a, a -> JE.Value)

{-| Type of a codec builder used with ```first```, ```next``` and ```end``` -}
type Builder a b = CB a b

{-| Codec matching and producing strings. -}
string : Codec String
string = (JD.string, JE.string)

{-| Codec matching and producing bools. -}
bool : Codec Bool
bool = (JD.bool, JE.bool)

{-| Codec matching and producing ints. -}
int : Codec Int
int = (JD.int, JE.int)

{-| Codec matching and producing floats. -}
float : Codec Float
float = (JD.float, JE.float)

{-| Codec that maps null to Nothing and vice versa. -}
nullable : Codec a -> Codec (Maybe a)
nullable d = (JD.nullable (Tuple.first d), (Maybe.map (Tuple.second d)) >> Maybe.withDefault JE.null)

{-| Codec that produces and consumes lists. -}
list : Codec a -> Codec (List a)
list d = (JD.list (Tuple.first d), JE.list << (List.map (Tuple.second d)))

{-| Codec that produces and consumes arrays. -}
array : Codec a -> Codec (Array a)
array d = (JD.array (Tuple.first d), JE.array << (Array.map (Tuple.second d)))

{-| Codec that produces and consumes dictionaries of other values. -}
dict : Codec a -> Codec (Dict String a)
dict d = (JD.dict (Tuple.first d), Dict.toList >> List.map (\(k,v) -> (k,(Tuple.second d) v)) >> JE.object)

{-| Codec that produces and consumes key value pair lists of other values. -}
keyValuePairs : Codec a -> Codec (List (String, a))
keyValuePairs d =
    (JD.keyValuePairs (Tuple.first d), List.map (\(k,v) -> (k,(Tuple.second d) v)) >> JE.object)

{-| Codec that matches a single field similar to Json.Decode and produces a
singleton object with 1 field. -}
singleton : String -> Codec a -> Codec a
singleton name d =
    (JD.field name (Tuple.first d), \v -> JE.object [(name, (Tuple.second d) v)])

{-| Codec that matches and produces objects with 2 given named fields. -}
object2 :
    (a -> b -> x) ->
    (x -> (a,b)) ->
    (String,Codec a) ->
    (String,Codec b) ->
    Codec x
object2 decodefun encodefun (n1,d1) (n2,d2) =
    ( JD.map2 decodefun (JD.field n1 (Tuple.first d1)) (JD.field n2 (Tuple.first d2))
    , \x ->
        let (a,b) = encodefun x in
        JE.object
            [ (n1,(Tuple.second d1) a)
            , (n2,(Tuple.second d2) b)
            ]
    )

{-| Codec that matches and produces objects with 3 given named fields. -}
object3 :
    (a -> b -> c -> x) ->
    (x -> (a,b,c)) ->
    (String,Codec a) ->
    (String,Codec b) ->
    (String,Codec c) ->
    Codec x
object3 decodefun encodefun (n1,d1) (n2,d2) (n3,d3) =
    ( JD.map3 decodefun (JD.field n1 (Tuple.first d1)) (JD.field n2 (Tuple.first d2)) (JD.field n3 (Tuple.first d3))
    , \x ->
        let (a,b,c) = encodefun x in
        JE.object
            [ (n1,(Tuple.second d1) a)
            , (n2,(Tuple.second d2) b)
            , (n3,(Tuple.second d3) c)
            ]
    )

{-| Codec that matches and produces objects with 4 given named fields. -}
object4 :
    (a -> b -> c -> d -> x) ->
    (x -> (a,b,c,d)) ->
    (String,Codec a) ->
    (String,Codec b) ->
    (String,Codec c) ->
    (String,Codec d) ->
    Codec x
object4 decodefun encodefun (n1,d1) (n2,d2) (n3,d3) (n4,d4) =
    ( JD.map4 decodefun
        (JD.field n1 (Tuple.first d1))
        (JD.field n2 (Tuple.first d2))
        (JD.field n3 (Tuple.first d3))
        (JD.field n4 (Tuple.first d4))
    , \x ->
        let (a,b,c,d) = encodefun x in
        JE.object
            [ (n1,(Tuple.second d1) a)
            , (n2,(Tuple.second d2) b)
            , (n3,(Tuple.second d3) c)
            , (n4,(Tuple.second d4) d)
            ]
    )

{-| Codec that matches and produces objects with 5 given named fields. -}
object5 :
    (a -> b -> c -> d -> e -> x) ->
    (x -> (a,b,c,d,e)) ->
    (String,Codec a) ->
    (String,Codec b) ->
    (String,Codec c) ->
    (String,Codec d) ->
    (String,Codec e) ->
    Codec x
object5 decodefun encodefun (n1,d1) (n2,d2) (n3,d3) (n4,d4) (n5,d5) =
    ( JD.map5 decodefun
        (JD.field n1 (Tuple.first d1))
        (JD.field n2 (Tuple.first d2))
        (JD.field n3 (Tuple.first d3))
        (JD.field n4 (Tuple.first d4))
        (JD.field n5 (Tuple.first d5))
    , \x ->
        let (a,b,c,d,e) = encodefun x in
        JE.object
            [ (n1,(Tuple.second d1) a)
            , (n2,(Tuple.second d2) b)
            , (n3,(Tuple.second d3) c)
            , (n4,(Tuple.second d4) d)
            , (n5,(Tuple.second d5) e)
            ]
    )

{-| Codec that matches and produces objects with 6 given named fields. -}
object6 :
    (a -> b -> c -> d -> e -> f -> x) ->
    (x -> (a,b,c,d,e,f)) ->
    (String,Codec a) ->
    (String,Codec b) ->
    (String,Codec c) ->
    (String,Codec d) ->
    (String,Codec e) ->
    (String,Codec f) ->
    Codec x
object6 decodefun encodefun (n1,d1) (n2,d2) (n3,d3) (n4,d4) (n5,d5) (n6,d6) =
    ( JD.map6 decodefun
        (JD.field n1 (Tuple.first d1))
        (JD.field n2 (Tuple.first d2))
        (JD.field n3 (Tuple.first d3))
        (JD.field n4 (Tuple.first d4))
        (JD.field n5 (Tuple.first d5))
        (JD.field n6 (Tuple.first d6))
    , \x ->
        let (a,b,c,d,e,f) = encodefun x in
        JE.object
            [ (n1,(Tuple.second d1) a)
            , (n2,(Tuple.second d2) b)
            , (n3,(Tuple.second d3) c)
            , (n4,(Tuple.second d4) d)
            , (n5,(Tuple.second d5) e)
            , (n6,(Tuple.second d6) f)
            ]
    )

{-| Codec that matches and produces objects with 7 given named fields. -}
object7 :
    (a -> b -> c -> d -> e -> f -> g -> x) ->
    (x -> (a,b,c,d,e,f,g)) ->
    (String,Codec a) ->
    (String,Codec b) ->
    (String,Codec c) ->
    (String,Codec d) ->
    (String,Codec e) ->
    (String,Codec f) ->
    (String,Codec g) ->
    Codec x
object7 decodefun encodefun (n1,d1) (n2,d2) (n3,d3) (n4,d4) (n5,d5) (n6,d6) (n7,d7) =
    ( JD.map7 decodefun
        (JD.field n1 (Tuple.first d1))
        (JD.field n2 (Tuple.first d2))
        (JD.field n3 (Tuple.first d3))
        (JD.field n4 (Tuple.first d4))
        (JD.field n5 (Tuple.first d5))
        (JD.field n6 (Tuple.first d6))
        (JD.field n7 (Tuple.first d7))
    , \x ->
        let (a,b,c,d,e,f,g) = encodefun x in
        JE.object
            [ (n1,(Tuple.second d1) a)
            , (n2,(Tuple.second d2) b)
            , (n3,(Tuple.second d3) c)
            , (n4,(Tuple.second d4) d)
            , (n5,(Tuple.second d5) e)
            , (n6,(Tuple.second d6) f)
            , (n7,(Tuple.second d7) g)
            ]
    )

{-| Codec that matches and produces objects with 8 given named fields. -}
object8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> x) ->
    (x -> (a,b,c,d,e,f,g,h)) ->
    (String,Codec a) ->
    (String,Codec b) ->
    (String,Codec c) ->
    (String,Codec d) ->
    (String,Codec e) ->
    (String,Codec f) ->
    (String,Codec g) ->
    (String,Codec h) ->
    Codec x
object8 decodefun encodefun (n1,d1) (n2,d2) (n3,d3) (n4,d4) (n5,d5) (n6,d6) (n7,d7) (n8,d8) =
    ( JD.map8 decodefun
        (JD.field n1 (Tuple.first d1))
        (JD.field n2 (Tuple.first d2))
        (JD.field n3 (Tuple.first d3))
        (JD.field n4 (Tuple.first d4))
        (JD.field n5 (Tuple.first d5))
        (JD.field n6 (Tuple.first d6))
        (JD.field n7 (Tuple.first d7))
        (JD.field n8 (Tuple.first d8))
    , \x ->
        let (a,b,c,d,e,f,g,h) = encodefun x in
        JE.object
            [ (n1,(Tuple.second d1) a)
            , (n2,(Tuple.second d2) b)
            , (n3,(Tuple.second d3) c)
            , (n4,(Tuple.second d4) d)
            , (n5,(Tuple.second d5) e)
            , (n6,(Tuple.second d6) f)
            , (n7,(Tuple.second d7) g)
            , (n8,(Tuple.second d8) h)
            ]
    )

{-| Wrap the codec in a transformer that produces and consumes another type.

    type Alternative = A | Unknown String

    altcoder : Codec Alternative
    altcoder =
        Codec.string
        |> Codec.map
            (\a -> if a == "A" then A else Unknown a)
            (\v ->
                case v of
                    A -> "A"
                    Unknown x -> x
            )

-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map f g d = (Tuple.first d |> JD.map f, \e -> (Tuple.second d) (g e))

{-| Match one of many decoders, encode using the given function. -}
oneOf : List (JD.Decoder a) -> (a -> JE.Value) -> Codec a
oneOf dl enc =
    (JD.oneOf dl, enc)

{-| Codec that matches null, produces null. -}
null : a -> Codec a
null v = (JD.null v, always JE.null)

{-| Codec that produces a constant decoded value and encodes to a constant value.
One might use this to check a field with a constant value, such as a version
number.
-}
succeed : a -> JE.Value -> Codec a
succeed vi vo = (JD.succeed vi, always vo)

{-| Codec that produces a constant encoded value but always fails decoding.
One might use this while prefiltering inputs based on their structure but ensure
that encoded json has the right structure.
-}
fail : String -> JE.Value -> Codec a
fail err vo = (JD.fail err, always vo)

{-| Like map, but the decode function returns a decoder that will be evaluated
next, rather than just mapping the value.

    -- An example showing the use of ```andThen``` to exclude
    -- bad values.
    pickycoder : Codec Picky
    pickycoder =
        JsonCodec.string |> JsonCodec.andThen
            (((flip Dict.get) pickyOpts)
            >> Maybe.map JD.succeed
            >> Maybe.withDefault (JD.fail "Option must be A or B")
            )
            (\p -> pickyOpts
            |> Dict.toList
            |> List.filterMap (\(k,v) -> if v == p then Just k else Nothing)
            |> List.head
            |> Maybe.withDefault "A"
            )
-}
andThen : (a -> JD.Decoder b) -> (b -> a) -> Codec a -> Codec b
andThen f g d = (Tuple.first d |> JD.andThen f, \e -> (Tuple.second d) (g e))

{-| Get a Json.Decode.Decoder from a codec. -}
decoder : Codec a -> JD.Decoder a
decoder = Tuple.first

{-| Get a function that encodes Json.Encode.Value from a codec. -}
encoder : Codec a -> (a -> JE.Value)
encoder = Tuple.second

{-| Construct an arbitrary codec from a decoder and an encoder function. -}
init : JD.Decoder a -> (a -> JE.Value) -> Codec a
init = (,)

{-| Application style codec creation.  Provide a field at a time to build up an
arbitrarily large codec.  Start with first, use next to add fields in the middle
and then convert it into a codec with end.

    type alias Test = { i : Int, b : Bool, f : Float, s : String }

    codec = 
        Test
        |> JC.first "i" JC.int .i
        |> JC.next "b" JC.bool .b
        |> JC.next "f" JC.float .f
        |> JC.end "s" JC.string .s

    x = JD.decodeString (JC.decoder be) "{\"i\":3,\"b\":false,\"f\":3.14,\"s\":\"hi there\"}"
    -- Ok { i = 3, b = False, f = 3.14, s = "hi there" }
-}
firstDec : String -> JD.Decoder a -> (a -> b) -> JD.Decoder b
firstDec field dec inp = JD.map inp (JD.field field dec)

restDec : String -> JD.Decoder x -> JD.Decoder (x -> y) -> JD.Decoder y
restDec field db da = JD.andThen (\sas -> JD.map sas (JD.field field db)) da

firstEnc : String -> (v -> JE.Value) -> (o -> v) -> (o -> List (String,JE.Value))
firstEnc field enc extract =
    \v -> [(field, enc (extract v))]

restEnc : String -> (v -> JE.Value) -> (o -> v) -> (o -> List (String,JE.Value)) -> (o -> List (String,JE.Value))
restEnc field enc extract prev =
    \v -> (field, enc (extract v)) :: (prev v)

{-| Start composing a codec to decode a record using a series of function
applications.

Using:

- A field name
- A function that decodes the value ```v``` in that field
- A function that extracts the field value ```v``` from the finished record
- A record building function such as a type alias constructor 
(we'll call this ```o```)

Return:

- A partially constructed decoder, which given the rest of the parameters 
for ```o```, yields a ```Codec o```.

You can build record codecs of arbitrarily many parameters with this, the same
way other codecs are built, together using the same code.

An example:

    type alias X = { i : Int, s : String, b : Bool, f : Float }
    c =  JC.first "i" JC.int .i X 
      |> JC.next "s" JC.string .s
      |> JC.next "b" JC.bool .b
      |> JC.next "f" JC.float .f
      |> JC.end

    > JD.decodeString (JC.decoder c) "{\"i\":3,\"s\":\"hi\",\"b\":false,\"f\":1.9}"
    Ok { i = 3, s = "hi", b = False, f = 1.9 } : Result.Result String Repl.X

-}
first : String -> Codec v -> (o -> v) -> (v -> b) -> Builder (JD.Decoder b) (o -> List (String, JE.Value))
first field cod extract inp =
    CB
        (firstDec field (decoder cod) inp)
        (firstEnc field (encoder cod) extract)

{-| Continue a partial codec from first, satisfying one more parameter of the
constructor function.
-}
next : String -> Codec v -> (o -> v) -> Builder (JD.Decoder (v -> b)) (o -> List (String, JE.Value)) -> Builder (JD.Decoder b) (o -> List (String, JE.Value))
next field cod extract (CB dd ee) =
    CB
        (restDec field (decoder cod) dd)
        (restEnc field (encoder cod) extract ee)

{-| Make the final step to turn a result from Builder into Codec. -}
end : Builder (JD.Decoder o) (o -> List (String,JE.Value)) -> Codec o
end (CB dec enc) =
    init dec (\v -> enc v |> JE.object)
