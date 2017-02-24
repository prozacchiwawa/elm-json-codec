module JsonCodec exposing
    ( Codec
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
    , fold
    , foldField
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

    -- An example of using fold to create the same codec.
    foldcoder : Codec Session
    foldcoder =
        let setQueue q s = { s | queue = q } in
        let setPlaying p s = { s | playing = p } in
        let setLikeCats l s = { s | likeCategories = l } in
        JsonCodec.fold
            [ JsonCodec.foldField "queue"
                (setQueue, JD.list JD.string)
                (.queue >> List.map JE.string >> JE.list)
            , JsonCodec.foldField "playing"
                (setPlaying, JsonCodec.decoder playingSerializer)
                (.playing >> JsonCodec.encoder playingSerializer)
            , JsonCodec.foldField "like"
                (setLikeCats, JD.dict JD.int)
                (.likeCategories
                >> Dict.map (always JE.int) >> Dict.toList >> JE.object
                )
            ]
            { queue = [], playing = Nothing, likeCategories = Dict.empty }

# Type
@docs Codec

# Simple codecs
@docs string, bool, int, float, nullable, list, array, dict, keyValuePairs, singleton, object2, object3, object4, object5, object6, object7, object8, null, succeed, fail

# Transform in both directions
@docs map, andThen

# Decoding alternatives
@docs oneOf

# Construction
@docs decoder, encoder, init

# Folding Construction
@docs fold, foldField

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

{-| Construct a field value for use with ```fold```.

* ```name``` specifies the name of a field that will be matched and produced.

* ```(update, decoder)``` specifies an update function and a decoder.
The update function is given the new value and the current state of the object
and returns a mutated object.

* ```extract``` specifies a function that encodes this field's json value.

-}
foldField : String -> (v -> a -> a, JD.Decoder v) -> (a -> JE.Value) -> (String, JD.Value -> a -> Result String a, a -> JE.Value)
foldField name (update,decoder) extract =
    ( name
    , \v a -> (JD.decodeValue (JD.field name decoder) v) |> Result.map (\v -> update v a)
    , extract
    )

{-| Produce a codec that uses a list of fields and codecs to fold new values
into an object, producing a new object with the json loaded on to it.

This can be used to consume delta updates from a channel, create codecs that
operate on objects with many fields, and create codecs with complex field
encodings.

    foldcoder : Codec Session
    foldcoder =
        let setQueue q s = { s | queue = q } in
        let setPlaying p s = { s | playing = p } in
        let setLikeCats l s = { s | likeCategories = l } in
        JsonCodec.fold
            [ JsonCodec.foldField "queue"
                (setQueue, JD.list JD.string)
                (.queue >> List.map JE.string >> JE.list)
            , JsonCodec.foldField "playing"
                (setPlaying, JsonCodec.decoder playingSerializer)
                (.playing >> JsonCodec.encoder playingSerializer)
            , JsonCodec.foldField "like"
                (setLikeCats, JD.dict JD.int)
                (.likeCategories
                >> Dict.map (always JE.int) >> Dict.toList >> JE.object
                )
            ]
            { queue = [], playing = Nothing, likeCategories = Dict.empty }

-}
fold : List (String, JD.Value -> a -> Result String a, a -> JE.Value) -> a -> Codec a
fold tups init =
    let
        foldfun : JD.Value -> (String, JD.Value -> a -> Result String a, a -> JE.Value) -> (Result String a) -> Result String a
        foldfun v (n,d,_) = Result.andThen (d v)
    in
    (   JD.value
    |>  JD.andThen
            (\v ->
                List.foldl (foldfun v) (Ok init) tups
                |> Result.map JD.succeed
                |> Result.mapError JD.fail
                |> Result.Extra.merge
            )
    ,   \v -> tups |> List.map (\(n,_,e) -> (n,e v)) |> JE.object
    )
