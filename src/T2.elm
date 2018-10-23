module T2 exposing (..)

import Dict as Dict
import Dict exposing (Dict)
import JsonCodec as JC
import JsonCodec exposing (Codec)

-- A codec that can stand on its own and also be reused.
playingSerializer : Codec (Maybe (String,Float))
playingSerializer =
    JsonCodec.nullable
        (JsonCodec.object2
             (\a b -> (a,b))
             ("u", JsonCodec.string, Tuple.first)
             ("p", JsonCodec.float, Tuple.second)
        )
            
type alias Session =
    { queue : List String
    , playing : Maybe (String,Float)
    , likeCategories : Dict String Int
    }

-- Simple codec built with composition.
serializer : Codec Session
serializer =
    JsonCodec.object3
        Session
        ("queue", JsonCodec.list JsonCodec.string, .queue)
        ("playing", playingSerializer, .playing)
        ("like", JsonCodec.dict JsonCodec.int, .likeCategories)
