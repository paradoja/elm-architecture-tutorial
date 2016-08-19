import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import List
import Regex

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : Int
  , showErrors : Bool
  }


model : Model
model =
  Model "" "" "" 0 False



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age Int
    | ShowValidations


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
        { model | name = name }

    Password password ->
        { model | password = password }

    PasswordAgain password ->
        { model | passwordAgain = password }

    Age age ->
        { model | age = age }

    ShowValidations ->
        { model | showErrors = True }

-- VIEW


getAgeInt : Int -> String -> Int
getAgeInt default ageString =
    case String.toInt ageString of
        Ok age ->
            age

        Err _ ->
            default

view : Model -> Html Msg
view model =
  div []
    [ div [] [ input [ type' "text", placeholder "Name", onInput Name ] []
             , input [ type' "password", placeholder "Password", onInput Password ] []
             , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
             , input [ type' "number", placeholder "Age", onInput (Age << getAgeInt 0) ] []
             ]
    , input [ type' "submit", name "Submit", onClick ShowValidations ] []
    , viewValidation model
    ]


type Validation
    = Valid
    | Invalid (List String)

type alias Validator = Model -> Validation

combineValidators : Validation -> Validation -> Validation
combineValidators v1 v2 =
    case v1 of
        Valid -> v2
        Invalid reasons1 ->
            case v2 of
                Valid -> v1
                Invalid reasons2 -> Invalid (reasons1 ++ reasons2)

checkPasswordAgain : Validator
checkPasswordAgain model =
    if model.password == model.passwordAgain then
        Valid
    else
        Invalid ["Passwords do not match!"]

checkPasswordLength : Int -> Validator
checkPasswordLength length model =
    if String.length model.password >= length then
        Valid
    else
        Invalid ["Password should be bigger than " ++ toString length ++ " chars!"]

checkPasswordDiversity : List String -> Validator
checkPasswordDiversity checks model =
    let checker re = Regex.contains (Regex.regex re) model.password
    in if List.all checker checks  then
           Valid
       else
           Invalid ["Password should be diverse!!!"]

validators : List (Model -> Validation)
validators =
    [ checkPasswordAgain
    , checkPasswordLength 8
    , checkPasswordDiversity ["[a-z]", "[A-Z]", "[0-9]"]
    ]

viewValidation : Model -> Html msg
viewValidation model =
  let
      validations = List.map ((|>) model) validators
      validP = List.foldl combineValidators Valid validations
      (color, messages) =
          case validP of
              Valid -> ("green", ["OK"])
              Invalid reasons ->
                  ("red", reasons)
  in
      if model.showErrors then
          div [ style [("color", color)] ] <|
              List.map (\m-> div [] [ text m ]) messages
      else
          text ""
