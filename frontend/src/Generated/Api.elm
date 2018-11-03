module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias User =
    { username : String
    , age : Int
    , email : String
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "username" string
        |> required "age" int
        |> required "email" string

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "username", Json.Encode.string x.username )
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

postUsers : User -> Http.Request (User)
postUsers body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000/api"
                , "users"
                ]
        , body =
            Http.jsonBody (encodeUser body)
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }