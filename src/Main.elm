import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Set
import Url exposing (Url, fromString)
import Url.Builder exposing (crossOrigin)
import Url.Parser exposing (map, (</>), string, parse)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = 
  {
    owner : String,
    name : String
  }

init : Model
init = Model "" ""

type Msg 
  = RepoNameUpdated String
  | SubmitForm


update : Msg -> Model -> Model
update msg model =
  case msg of 
    RepoNameUpdated pathOrUrl ->
      parseRepoName pathOrUrl
    SubmitForm ->
      Model "" ""

parseRepoName : String -> Model
parseRepoName urlString =
  case (fromString urlString) of
    Nothing ->
      Model "" ""
    Just url ->
      Maybe.withDefault (Model "" "") (parse (map Model (string </> string)) url)

view : Model -> Html Msg
view model =
  Html.form 
    [ class "form-container" ]
    [
      label [] 
      [ 
        text "Github Repo",
        input [type_ "text", placeholder "URL or unique name", onInput RepoNameUpdated] []
      ],
      viewValidation model,
      viewButton model
    ]

viewValidation : Model -> Html msg
viewValidation model =
  case (model.owner, model.name) of
    ("", "") ->
      div [ style "color" "red" ] [ text "Invalid Github Repo" ]
    _ ->
      div [ style "color" "green" ] [ text ("Valid Github Repo: " ++ model.owner ++ "/" ++ model.name) ]

viewButton : Model -> Html msg
viewButton model =
  case (model.owner, model.name) of
    ("", "") ->
      button [disabled True] [text "Submit"]
    _ ->
      button [] [text "Submit"]