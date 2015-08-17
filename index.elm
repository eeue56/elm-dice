import Html exposing (div, button, text, input, fromElement, form)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (type', value)

import Graphics.Element exposing (show)
import String exposing (toInt)
import Result exposing (Result(..))

import StartApp.Simple as StartApp

import Random exposing (generate, initialSeed, int)

main =
  StartApp.start { model = model, view = view, update = update }


type alias Model = {
    number : Int,
    seed : Random.Seed,
    min : Int,
    max : Int
}

{- 
  Every time you make a call to the random generator, you must pass a seed.
  There's two ways of getting the seed - either from a call to the generator, 
  or by calling initialSeed with some value.
  
  Here we store a seed in our model, along with the current number.
-}
model : Model
model = { number = 0,  
  seed = Random.initialSeed 5,
  min = 1,
  max = 6 }


updateNumber n model = { model | number <- n } 
updateSeed s model = { model | seed <- s }

{- 
  The random number generator takes Generator a, 
  where a is the type of the value to be produced.
  
  The generators provided such as int take a min and max value to generate
  between
-}
newNumber : Model -> Model
newNumber model = 
  let
    (number, seed) = generate (int model.min model.max) model.seed
  in
     updateNumber number <| updateSeed seed model 

numberView : (String -> Signal.Message) -> Int -> Html.Html
numberView action n = 
  input [ value (toString n), type' "number", on "input" targetValue action ]  [ text <| toString n ]

{-
  Try to convert a string to a number, given a default value
  Default to the value if it failed to convert
-}
attemptToInt : Int -> String -> Int
attemptToInt old newNumber = 
  case toInt newNumber of
    Ok x -> x
    Err _ -> old

minView address model = 
  form [] 
    [ text "Minimum:"
    , numberView (Signal.message address << NewMin << (attemptToInt model.min)) model.min
    ]

maxView address model = 
  form []
    [ text "Maximum:"
    , numberView (Signal.message address << NewMax << (attemptToInt model.max)) model.max
    ]

view address model =
  form []
    [ button [ onClick address NewNumber ] [ text "New random number!" ]
    , minView address model
    , maxView address model
    , div [] [ text <| "Current number: " ++ (toString model.number) ]
    ]

type Action = NewNumber | NewMin Int | NewMax Int

update action model =
  case action of
    NewNumber -> newNumber model
    NewMin x -> { model | min <- x }
    NewMax x -> { model | max <- x }