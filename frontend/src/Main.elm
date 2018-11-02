module Main exposing (main)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Generated.Api exposing (..)

type alias Model = 
    { users : List User }

type Msg = SetUsers (Result Http.Error (List User))
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        SetUsers newUsers -> 
            ({model | users = Result.withDefault model.users newUsers}, Cmd.none)
        
fetchUsers : Cmd Msg
fetchUsers =
  getUsers
    |> Http.send SetUsers

                
init : (Model, Cmd Msg)
init = ({ users = [] }
       , fetchUsers)


userTable : List User -> Html Msg
userTable users = 
    let header = tr [] [ th [] [text "Name"]
                       , th [] [text "Age"]
                       , th [] [text "Email"] ]
                 
        userRow user = tr [] [ td [] [text user.name]
                             , td [] [text (user.age |> toString)]
                             , td [] [text user.email] ]
    in
    table [ class "table table-striped" ]
        (header 
        :: List.map userRow users)
    
    
view : Model -> Html Msg
view model = 
    div [ class "container" ] 
        [ h1 [] [ text "An amazing Servant Elm demo"]
        , userTable model.users ]
    
main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
