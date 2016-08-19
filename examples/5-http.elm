import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Maybe exposing (..)
import String
import Task

main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  , error : Maybe String
  }


init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" Nothing
  , getRandomGif topic
  )



-- UPDATE


type Msg
  = MorePlease
  | Topic String
  | FetchSucceed String
  | FetchFail Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      MorePlease ->
          (model, getRandomGif model.topic)

      Topic topic ->
          ({ model | topic = topic }, Cmd.none)

      FetchSucceed newUrl ->
          (Model model.topic newUrl Nothing, Cmd.none)

      FetchFail httpError ->
          ({ model | error = Just <|decodeError httpError }, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ value model.topic, onInput Topic ] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    , br [] []
    , showError model.error
    ]

showError : Maybe String -> Html Msg
showError =
    text << Maybe.withDefault ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "//api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)


decodeGifUrl : Json.Decoder String
decodeGifUrl =
  Json.at ["data", "image_url"] Json.string

decodeError : Http.Error -> String
decodeError error =
    case error of
        Http.Timeout -> "Timeout!"
        Http.NetworkError -> "Network error!"
        Http.UnexpectedPayload str -> "Wrong JSON " ++ str ++ "!"
        Http.BadResponse n str -> String.join " " ["Error ", toString n, str, "!"]
