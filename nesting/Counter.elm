module Counter exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import String


-- MODEL


type alias Model =
    { count : Int
    , maximum : Int
    , minimum : Int
    , clicks : Int }


init : Int -> Model
init count =
  { count = count
  , maximum = count
  , minimum = count
  , clicks = 0 }



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      addToModel 1 model

    Decrement ->
      addToModel -1 model

addToModel : Int -> Model -> Model
addToModel sum {count, maximum, minimum, clicks} =
    let newCount = count + sum
    in { count = newCount
       , maximum = Basics.max newCount maximum
       , minimum = Basics.min newCount minimum
       , clicks = clicks + 1 }

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model.count) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [] [ text <| String.join "," <|
                   List.map toString  [model.maximum, model.minimum, model.clicks] ]
    ]


countStyle : Attribute msg
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
