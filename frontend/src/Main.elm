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
         | DeletedUser Int (Result Http.Error String)
         | ChangeUsername String
         | ChangeAge String
         | ChangeEmail String
         | SubmitUser
         | DeleteUser Int
    
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
        DeleteUser id -> (model, deleteUser id)
        DeletedUser id result ->
            case result of
                Ok msg -> 
                    let 
                        users = List.filter (\u -> u.id /= Just id) model.users 
                    in
                        ({model | users = users}, Cmd.none)
                          
                Err err -> (model, Cmd.none) -- TODO handle
            
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
                   
deleteUser : Int -> Cmd Msg
deleteUser id  = deleteUsersByUserId id
                 |> Http.send (DeletedUser id)

emptyUser : User
emptyUser = { id = Nothing
            , username = ""
            , age = 0
            , email = "" }
                
init : (Model, Cmd Msg)
init = ({ users = []
        , newUser = emptyUser
        }
       , fetchUsers)


userTable : List User -> Html Msg
userTable users = 
    let header = tr [] [ th [] [text "Name"]
                       , th [] [text "Age"]
                       , th [] [text "Email"]
                       , th [] []
                       ]
                 
        userRow user = tr [] [ td [] [text user.username]
                             , td [] [text (user.age |> toString)]
                             , td [] [text user.email]
                             , td [] [ case user.id of
                                           Nothing -> text ""
                                           Just id -> button [ class "btn btn-danger"
                                                             , onClick (DeleteUser id) ]
                                                      [ text "Delete" ]
                                     ]
                             ]
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
