module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias User =
    { name : String
    , age : Int
    , email : String
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "name" string
        |> required "age" int
        |> required "email" string

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "age", Json.Encode.int x.age )
        , ( "email", Json.Encode.string x.email )
        ]

getUsers : Http.Request (List (User))
getUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000/api"
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }