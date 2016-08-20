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
    let calculateHand f time length =
            let angle = turns (f time) - Basics.pi/2
                x = (50 + length * cos angle)
                y = (50 + length * sin angle)
            in { x= toString x, y= toString y}
        minuteHand = calculateHand Time.inMinutes model.time 40
        hourHand = calculateHand Time.inHours model.time 30
    in Html.div [] [
         svg [ viewBox "0 0 100 100", width "300px" ]
             [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
             , line [ x1 "50", y1 "50", x2 minuteHand.x, y2 minuteHand.y, stroke "#023963" ] []
             , line [ x1 "50", y1 "50", x2 hourHand.x, y2 hourHand.y, stroke "white" ] []
             ]
        , pauseButton model ]

pauseButton : Model -> Html Msg
pauseButton {active} =
    let pauseText =
            if active then "Pause"
            else "Play"
    in Html.button [ onClick Pause ] [ text pauseText ]
