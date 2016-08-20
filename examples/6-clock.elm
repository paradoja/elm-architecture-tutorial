import Html exposing (Html)
import Html.App as Html
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
    { time: Time
    , active: Bool }


init : (Model, Cmd Msg)
init =
  (Model 0 True, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | Pause


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Tick _ ->
          if model.active then
              ({ model | time = model.time + 1000 }, Cmd.none)
          else
              (model, Cmd.none)

      Pause ->
          ({ model | active = not model.active }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    angleMin =
      turns (Time.inMinutes model.time) - Basics.pi/2

    minHandX =
      toString (50 + 40 * cos angleMin)

    minHandY =
      toString (50 + 40 * sin angleMin)

    angleHours =
      turns (Time.inHours model.time) - Basics.pi/2

    hoursHandX =
      toString (50 + 30 * cos angleHours)

    hoursHandY =
      toString (50 + 30 * sin angleHours)
  in
      Html.div [] [
           svg [ viewBox "0 0 100 100", width "300px" ]
               [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
               , line [ x1 "50", y1 "50", x2 minHandX, y2 minHandY, stroke "#023963" ] []
               , line [ x1 "50", y1 "50", x2 hoursHandX, y2 hoursHandY, stroke "white" ] []
               ]
          , pauseButton model ]

pauseButton : Model -> Html Msg
pauseButton {active} =
    let pauseText =
            if active then "Pause"
            else "Play"
    in Html.button [ onClick Pause ] [ text pauseText ]
