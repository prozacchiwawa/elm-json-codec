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
    , firstOpt
    , next
    , start
    , option
    , end
    )

{-| Build json decoders and encoders simultaneously, conserving the need for
functions that do each in simple cases.  This allows a single datum to embody
both the encoder and the decoder for a type, reducing some duplication and
redundancy.  In simple cases, a codec can be built exclusively by composing
other codecs.

Contributions by [francescortiz](https://github.com/francescortiz)

In more complex cases, ```first``` could be used, which allows fields to be
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
                 (\a b -> (a,b))
                 ("u", JsonCodec.string, Tuple.first)
                 ("p", JsonCodec.float, Tuple.second)
            )

    -- Simple codec built with composition.
    serializer : Codec Session
    serializer =
        JsonCodec.object3
            Session
            ("queue", JsonCodec.list JsonCodec.string, .queue)
            ("playing", playingSerializer, .playing)
            ("like", JsonCodec.dict JsonCodec.int, .likeCategories)

    -- Codec built in application style
    type alias Test = 
        { i : Int, b : Bool, f : Float, o : Maybe String, s : String }

    codec = 
        Test
        |> JC.first  "i" JC.int .i
        |> JC.next   "b" JC.bool .b
        |> JC.next   "f" JC.float .f
        |> JC.option "o" (JC.nullable JC.string) .o Nothing
        |> JC.next   "s" JC.string .s
        |> JC.end

    x = JD.decodeString (JC.decoder be) 
         "{\"i\":3,\"b\":false,\"f\":3.14,\"s\":\"hi there\"}"
    -- Ok { b = False, f = 3.14, i = 3, o = Nothing, s = "hi there" }
    y = JD.decodeString (JC.decoder codec)
        "{\"i\":3,\"b\":false,\"f\":3.14,\"o\":\"hi\",\"s\":\"hi there\"}"
    -- Ok { b = False, f = 3.14, i = 3, o = Just "hi", s = "hi there" }
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
@docs start, first, firstOpt, next, option, end

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE

{-| The type of codecs constructed by the library.

You can extract a Json.Decode.Decoder with ```decoder``` and a function that
constructs ```Json.Encoder.Value``` with ```encoder```.
-}
type Codec a = Codec (JD.Decoder a, a -> JE.Value)

{-| Type of a codec builder used with ```first```, ```next``` and ```end``` -}
type Builder a b = CB a b

{-| Codec matching and producing strings. -}
string : Codec String
string = Codec (JD.string, JE.string)

{-| Codec matching and producing bools. -}
bool : Codec Bool
bool = Codec (JD.bool, JE.bool)

{-| Codec matching and producing ints. -}
int : Codec Int
int = Codec (JD.int, JE.int)

{-| Codec matching and producing floats. -}
float : Codec Float
float = Codec (JD.float, JE.float)

{-| Codec that maps null to Nothing and vice versa. -}
nullable : Codec a -> Codec (Maybe a)
nullable d = Codec (JD.nullable (decoder d), (Maybe.map (encoder d)) >> Maybe.withDefault JE.null)

{-| Codec that produces and consumes lists. -}
list : Codec a -> Codec (List a)
list d =
    Codec (JD.list (decoder d), JE.list (encoder d))

{-| Codec that produces and consumes arrays. -}
array : Codec a -> Codec (Array a)
array d =
    Codec (JD.array (decoder d), JE.array (encoder d))

{-| Codec that produces and consumes dictionaries of other values. -}
dict : Codec a -> Codec (Dict String a)
dict d = Codec (JD.dict (decoder d), Dict.toList >> List.map (\(k,v) -> (k,(encoder d) v)) >> JE.object)

{-| Codec that produces and consumes key value pair lists of other values. -}
keyValuePairs : Codec a -> Codec (List (String, a))
keyValuePairs d =
    Codec (JD.keyValuePairs (decoder d), List.map (\(k,v) -> (k,(encoder d) v)) >> JE.object)

{-| Codec that matches a single field similar to Json.Decode and produces a
singleton object with 1 field. -}
singleton : String -> Codec a -> Codec a
singleton name d =
    Codec (JD.field name (decoder d), \v -> JE.object [(name, (encoder d) v)])

{-| Codec that matches and produces objects with 2 given named fields. -}
object2 :
    (a -> b -> x) ->
    (String,Codec a,x -> a) ->
    (String,Codec b,x -> b) ->
    Codec x
object2 decodefun (n1,d1,x1) (n2,d2,x2) =
    Codec
    ( JD.map2 decodefun (JD.field n1 (decoder d1)) (JD.field n2 (decoder d2))
    , \x ->
        JE.object
            [ (n1,(encoder d1) (x1 x))
            , (n2,(encoder d2) (x2 x))
            ]
    )

{-| Codec that matches and produces objects with 3 given named fields. -}
object3 :
    (a -> b -> c -> x) ->
    (String,Codec a,x -> a) ->
    (String,Codec b,x -> b) ->
    (String,Codec c,x -> c) ->
    Codec x
object3 decodefun (n1,d1,x1) (n2,d2,x2) (n3,d3,x3) =
    Codec
    ( JD.map3 decodefun (JD.field n1 (decoder d1)) (JD.field n2 (decoder d2)) (JD.field n3 (decoder d3))
    , \x ->
        JE.object
            [ (n1,(encoder d1) (x1 x))
            , (n2,(encoder d2) (x2 x))
            , (n3,(encoder d3) (x3 x))
            ]
    )

{-| Codec that matches and produces objects with 4 given named fields. -}
object4 :
    (a -> b -> c -> d -> x) ->
    (String,Codec a, x -> a) ->
    (String,Codec b, x -> b) ->
    (String,Codec c, x -> c) ->
    (String,Codec d, x -> d) ->
    Codec x
object4 decodefun (n1,d1,x1) (n2,d2,x2) (n3,d3,x3) (n4,d4,x4) =
    Codec
    ( JD.map4 decodefun
        (JD.field n1 (decoder d1))
        (JD.field n2 (decoder d2))
        (JD.field n3 (decoder d3))
        (JD.field n4 (decoder d4))
    , \x ->
        JE.object
            [ (n1,(encoder d1) (x1 x))
            , (n2,(encoder d2) (x2 x))
            , (n3,(encoder d3) (x3 x))
            , (n4,(encoder d4) (x4 x))
            ]
    )

{-| Codec that matches and produces objects with 5 given named fields. -}
object5 :
    (a -> b -> c -> d -> e -> x) ->
    (String,Codec a,x -> a) ->
    (String,Codec b,x -> b) ->
    (String,Codec c,x -> c) ->
    (String,Codec d,x -> d) ->
    (String,Codec e,x -> e) ->
    Codec x
object5 decodefun (n1,d1,x1) (n2,d2,x2) (n3,d3,x3) (n4,d4,x4) (n5,d5,x5) =
    Codec
    ( JD.map5 decodefun
        (JD.field n1 (decoder d1))
        (JD.field n2 (decoder d2))
        (JD.field n3 (decoder d3))
        (JD.field n4 (decoder d4))
        (JD.field n5 (decoder d5))
    , \x ->
        JE.object
            [ (n1,(encoder d1) (x1 x))
            , (n2,(encoder d2) (x2 x))
            , (n3,(encoder d3) (x3 x))
            , (n4,(encoder d4) (x4 x))
            , (n5,(encoder d5) (x5 x))
            ]
    )

{-| Codec that matches and produces objects with 6 given named fields. -}
object6 :
    (a -> b -> c -> d -> e -> f -> x) ->
    (String,Codec a,x -> a) ->
    (String,Codec b,x -> b) ->
    (String,Codec c,x -> c) ->
    (String,Codec d,x -> d) ->
    (String,Codec e,x -> e) ->
    (String,Codec f,x -> f) ->
    Codec x
object6 decodefun (n1,d1,x1) (n2,d2,x2) (n3,d3,x3) (n4,d4,x4) (n5,d5,x5) (n6,d6,x6) =
    Codec
    ( JD.map6 decodefun
        (JD.field n1 (decoder d1))
        (JD.field n2 (decoder d2))
        (JD.field n3 (decoder d3))
        (JD.field n4 (decoder d4))
        (JD.field n5 (decoder d5))
        (JD.field n6 (decoder d6))
    , \x ->
        JE.object
            [ (n1,(encoder d1) (x1 x))
            , (n2,(encoder d2) (x2 x))
            , (n3,(encoder d3) (x3 x))
            , (n4,(encoder d4) (x4 x))
            , (n5,(encoder d5) (x5 x))
            , (n6,(encoder d6) (x6 x))
            ]
    )

{-| Codec that matches and produces objects with 7 given named fields. -}
object7 :
    (a -> b -> c -> d -> e -> f -> g -> x) ->
    (String,Codec a,x -> a) ->
    (String,Codec b,x -> b) ->
    (String,Codec c,x -> c) ->
    (String,Codec d,x -> d) ->
    (String,Codec e,x -> e) ->
    (String,Codec f,x -> f) ->
    (String,Codec g,x -> g) ->
    Codec x
object7 decodefun (n1,d1,x1) (n2,d2,x2) (n3,d3,x3) (n4,d4,x4) (n5,d5,x5) (n6,d6,x6) (n7,d7,x7) =
    Codec
    ( JD.map7 decodefun
        (JD.field n1 (decoder d1))
        (JD.field n2 (decoder d2))
        (JD.field n3 (decoder d3))
        (JD.field n4 (decoder d4))
        (JD.field n5 (decoder d5))
        (JD.field n6 (decoder d6))
        (JD.field n7 (decoder d7))
    , \x ->
        JE.object
            [ (n1,(encoder d1) (x1 x))
            , (n2,(encoder d2) (x2 x))
            , (n3,(encoder d3) (x3 x))
            , (n4,(encoder d4) (x4 x))
            , (n5,(encoder d5) (x5 x))
            , (n6,(encoder d6) (x6 x))
            , (n7,(encoder d7) (x7 x))
            ]
    )

{-| Codec that matches and produces objects with 8 given named fields. -}
object8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> x) ->
    (String,Codec a,x -> a) ->
    (String,Codec b,x -> b) ->
    (String,Codec c,x -> c) ->
    (String,Codec d,x -> d) ->
    (String,Codec e,x -> e) ->
    (String,Codec f,x -> f) ->
    (String,Codec g,x -> g) ->
    (String,Codec h,x -> h) ->
    Codec x
object8 decodefun (n1,d1,x1) (n2,d2,x2) (n3,d3,x3) (n4,d4,x4) (n5,d5,x5) (n6,d6,x6) (n7,d7,x7) (n8,d8,x8) =
    Codec
    ( JD.map8 decodefun
        (JD.field n1 (decoder d1))
        (JD.field n2 (decoder d2))
        (JD.field n3 (decoder d3))
        (JD.field n4 (decoder d4))
        (JD.field n5 (decoder d5))
        (JD.field n6 (decoder d6))
        (JD.field n7 (decoder d7))
        (JD.field n8 (decoder d8))
    , \x ->
        JE.object
            [ (n1,(encoder d1) (x1 x))
            , (n2,(encoder d2) (x2 x))
            , (n3,(encoder d3) (x3 x))
            , (n4,(encoder d4) (x4 x))
            , (n5,(encoder d5) (x5 x))
            , (n6,(encoder d6) (x6 x))
            , (n7,(encoder d7) (x7 x))
            , (n8,(encoder d8) (x8 x)) 
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
map f g d = Codec (decoder d |> JD.map f, \e -> (encoder d) (g e))

{-| Match one of many decoders, encode using the given function. -}
oneOf : List (JD.Decoder a) -> (a -> JE.Value) -> Codec a
oneOf dl enc =
    Codec (JD.oneOf dl, enc)

{-| Codec that matches null, produces null. -}
null : a -> Codec a
null v = Codec (JD.null v, always JE.null)

{-| Codec that produces a constant decoded value and encodes to a constant value.
One might use this to check a field with a constant value, such as a version
number.
-}
succeed : a -> JE.Value -> Codec a
succeed vi vo = Codec (JD.succeed vi, always vo)

{-| Codec that produces a constant encoded value but always fails decoding.
One might use this while prefiltering inputs based on their structure but ensure
that encoded json has the right structure.
-}
fail : String -> JE.Value -> Codec a
fail err vo = Codec (JD.fail err, always vo)

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
andThen f g d =
    Codec (decoder d |> JD.andThen f, \e -> (encoder d) (g e))

{-| Get a Json.Decode.Decoder from a codec. -}
decoder : Codec a -> JD.Decoder a
decoder (Codec (d,e)) = d

{-| Get a function that encodes Json.Encode.Value from a codec. -}
encoder : Codec a -> (a -> JE.Value)
encoder (Codec (d,e)) = e

{-| Construct an arbitrary codec from a decoder and an encoder function. -}
init : JD.Decoder a -> (a -> JE.Value) -> Codec a
init a b = Codec (a,b)

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
startDec :  b -> JD.Decoder b
startDec  inp = (JD.succeed inp)


firstDec : String -> JD.Decoder a -> (a -> b) -> JD.Decoder b
firstDec field dec inp = JD.map inp (JD.field field dec)

restDec : String -> JD.Decoder x -> JD.Decoder (x -> y) -> JD.Decoder y
restDec field db da = JD.andThen (\sas -> JD.map sas (JD.field field db)) da


startEnc :  (o -> List (String,JE.Value))
startEnc  =
    \v -> []


firstEnc : String -> (v -> JE.Value) -> (o -> v) -> (o -> List (String,JE.Value))
firstEnc field enc extract =
    \v -> [(field, enc (extract v))]


restEnc : String -> (v -> JE.Value) -> (o -> v) -> (o -> List (String,JE.Value)) -> (o -> List (String,JE.Value))
restEnc field enc extract prev =
    \v -> (field, enc (extract v)) :: (prev v)


{-| Start composing a codec.
-}
start :  b -> Builder (JD.Decoder b) (o -> List (String, JE.Value))
start  inp =
    CB
        (startDec inp)
        (startEnc )


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

{-| Begin decoding with an optional field.  As ```first``` but a default value is added. -}
firstOpt : String -> Codec v -> (o -> v) -> v -> (v -> b) -> Builder (JD.Decoder b) (o -> List (String, JE.Value))
firstOpt field cod extract def inp =
    let
        hasField db =
            JD.oneOf
                [ JD.field field db
                , JD.succeed def
                ]

        firstDO dec i = JD.map i (hasField dec)
    in
    CB
        (firstDO        (decoder cod) inp)
        (firstEnc field (encoder cod) extract)
            
{-| Continue a partial codec from first, satisfying one more parameter of the
constructor function.
-}
next : String -> Codec v -> (o -> v) -> Builder (JD.Decoder (v -> b)) (o -> List (String, JE.Value)) -> Builder (JD.Decoder b) (o -> List (String, JE.Value))
next field cod extract (CB dd ee) =
    CB
        (restDec field (decoder cod) dd)
        (restEnc field (encoder cod) extract ee)

{-| Allow pipelines to decode optional fields, not just optional values. -}
option : String -> Codec v -> (o -> v) -> v -> Builder (JD.Decoder (v -> b)) (o -> List (String, JE.Value)) -> Builder (JD.Decoder b) (o -> List (String, JE.Value))
option field cod extract default (CB dd ee) =
    let
        hasField db =
            JD.oneOf
                [ JD.field field db
                , JD.succeed default
                ]
        
        restOpt db da =
            JD.andThen
                (\sas ->
                     JD.map sas (hasField db)
                )
                da
    in
    CB
        (restOpt (decoder cod) dd)
        (restEnc field (encoder cod) extract ee)
    
{-| Make the final step to turn a result from Builder into Codec. -}
end : Builder (JD.Decoder o) (o -> List (String,JE.Value)) -> Codec o
end (CB dec enc) =
    init dec (\v -> enc v |> JE.object)
