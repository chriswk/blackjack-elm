module Main (..) where

import Components.Blackjack exposing (..)
import Components.Deck exposing (newDeck)
import Random exposing (Seed)
import Html exposing (..)
import Effects exposing (Effects, Never)
import Task
import StartApp
import Random exposing (initialSeed)


initialModel : Model
initialModel =
  { deck = []
  , seed = startTimeSeed
  , playerWins = 0
  , dealerWins = 0
  , gamesPlayed = 0
  , player = Player [] 0 Playing
  , dealer = Dealer [] 0 Playing 17
  }


startTimeSeed : Seed
startTimeSeed =
  initialSeed <| round startTime


init : ( Model, Effects Action )
init =
  let
    deck =
      newDeck

    m =
      { initialModel | deck = deck }

    model =
      initialDeal (shuffleCards m)
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


port startTime : Float



-- HOT-SWAPPING


port swap : Signal.Signal Bool