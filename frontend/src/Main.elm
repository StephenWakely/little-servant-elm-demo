module Main exposing (main)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Generated.Api exposing (..)

type alias Model = 
    { users : List User
    , newUser : User
    }

type Msg = SetUsers (Result Http.Error (List User))
         | AddUser (Result Http.Error User)
         | ChangeUsername String
         | ChangeAge String
         | ChangeEmail String
         | SubmitUser
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        SetUsers newUsers -> 
            ({ model | users = Result.withDefault model.users newUsers }, Cmd.none)
        SubmitUser -> (model, submitUser model.newUser)
        AddUser user ->
            case user of
                Ok user -> let users = user :: model.users in
                           ({model | users = users}, Cmd.none)
                Err err -> (model, Cmd.none) -- TODO handle the error
        ChangeUsername name -> 
            let new = model.newUser in
            ({ model | newUser = { new | username = name }}
            , Cmd.none)
        ChangeAge age -> let new = model.newUser in
                         ({ model | 
                            newUser = { new | age = String.toInt age |> Result.toMaybe |> Maybe.withDefault 0 }}
                         , Cmd.none)
        ChangeEmail email -> let new = model.newUser in
                             ({model | 
                               newUser = { new | email = email }}, Cmd.none)
        
fetchUsers : Cmd Msg
fetchUsers = getUsers
           |> Http.send SetUsers
       
submitUser : User -> Cmd Msg
submitUser user = postUsers user
                |> Http.send AddUser

emptyUser : User
emptyUser = { username = "", age = 0, email = "" }
                
init : (Model, Cmd Msg)
init = ({ users = []
        , newUser = emptyUser
        }
       , fetchUsers)


userTable : List User -> Html Msg
userTable users = 
    let header = tr [] [ th [] [text "Name"]
                       , th [] [text "Age"]
                       , th [] [text "Email"] ]
                 
        userRow user = tr [] [ td [] [text user.username]
                             , td [] [text (user.age |> toString)]
                             , td [] [text user.email] ]
    in
    table [ class "table table-striped" ]
        (header 
        :: List.map userRow users)
    
newUserForm : Html Msg
newUserForm = 
    div [ class "w-50" ]
        [ div [ class "form-group"]
              [ label [] [text "Username" ]
              , input [ class "form-control"
                      , onInput ChangeUsername ] []
              ]
        , div [ class "form-group"]
            [ label [] [text "Age" ]
            , input [ class "form-control"
                    , onInput ChangeAge ] []
            ]
        , div [ class "form-group" ]
            [ label [] [text "Email" ]
            , input [ class "form-control"
                    , onInput ChangeEmail ] []
            ]
        , button [ class "btn btn-success"
                 , onClick SubmitUser
                 ] [ text "Submit" ]
                       
        ]
    
view : Model -> Html Msg
view model = 
    div [ class "container" ] 
        [ h1 [] [ text "An amazing Servant Elm demo"]
        , userTable model.users
        , h2 [] [ text "New user"]
        , newUserForm ]
    
main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
