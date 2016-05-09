module Main (..) where

import Components.Blackjack exposing (..)
import Signal exposing (Signal, map)
import Random exposing (Seed)
import Html exposing (..)
import Effects exposing (Effects, Never)
import Task
import StartApp
import Random exposing (initialSeed)


initialModel : Model
initialModel =
  { deck = []
  , seed = initialSeed 5152
  }


init : ( Model, Effects Action )
init =
  let
    deck =
      newDeck

    m =
      { initialModel | deck = deck }

    model =
      shuffleCards m
  in
    ( model, Effects.none )


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , inputs = []
    , update = update
    , view = view
    }


main =
  app.html


port runner : Signal (Task.Task Never ())
port runner =
  app.tasks



-- HOT-SWAPPING


port swap : Signal.Signal Bool
