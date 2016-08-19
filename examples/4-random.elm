import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import List
import Random
import Svg
import Svg.Attributes exposing (..)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { dieFace : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 1, Random.generate NewFace (Random.int 1 6))



-- UPDATE


type Msg
  = Roll
  | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      (Model newFace, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ drawDice model.dieFace
    , br [] []
    , button [ onClick Roll ] [ text "Roll" ]
    ]

drawDice : Int -> Html Msg
drawDice number =
    let circle x y = Svg.circle [ cx <| toString x, cy <| toString y, r "10", fill "white" ] []
        faces = case number of
                    1 -> [ circle 60 60 ]
                    2 -> [ circle 35 35, circle 85 85 ]
                    3 -> [ circle 35 35, circle 60 60, circle 85 85 ]
                    4 -> [ circle 35 35, circle 85 85, circle 35 85, circle 85 35 ]
                    5 -> [ circle 35 35, circle 85 85, circle 35 85, circle 85 35, circle 60 60 ]
                    6 -> [ circle 35 35, circle 85 85, circle 35 85
                         , circle 85 35, circle 35 60, circle 85 60 ]
                    _ -> []
        rectangle = Svg.rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "red" ] []
    in Svg.svg [ width "120", height "120", viewBox "0 0 120 120" ] <| rectangle :: faces
