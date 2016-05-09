module Components.Blackjack (..) where

import Html exposing (..)
import Random exposing (Seed, generate)
import Signal exposing (Signal, Address)
import Effects exposing (Effects)
import Array exposing (toList, fromList)
import Random.Array exposing (shuffle)


type Action
  = NoOp


type Suit
  = Diamonds
  | Hearts
  | Spades
  | Clubs
  | None


type Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  | Joker


type alias Card =
  { suit : Suit
  , rank : Rank
  }


type alias Deck =
  List Card


type alias Model =
  { deck : Deck
  , seed : Seed
  }


suitFromString : String -> Suit
suitFromString suit =
  case suit of
    "Clubs" ->
      Clubs

    "Diamonds" ->
      Diamonds

    "Spades" ->
      Spades

    "Hearts" ->
      Hearts

    _ ->
      None


rankFromNumber : Int -> Rank
rankFromNumber rank =
  case rank of
    1 ->
      Ace

    2 ->
      Two

    3 ->
      Three

    4 ->
      Four

    5 ->
      Five

    6 ->
      Six

    7 ->
      Seven

    8 ->
      Eight

    9 ->
      Nine

    10 ->
      Ten

    11 ->
      Jack

    12 ->
      Queen

    13 ->
      King

    _ ->
      Joker


rankForSuit : Suit -> List Card
rankForSuit suit =
  let
    ranks =
      List.map rankFromNumber [1..13]

    partialDeck =
      List.map (\r -> Card suit r) ranks
  in
    partialDeck


newDeck : Deck
newDeck =
  let
    suits =
      List.map suitFromString [ "Clubs", "Diamonds", "Hearts", "Spades" ]

    deck =
      List.concatMap rankForSuit suits
  in
    deck


valueOfCard : Card -> Int
valueOfCard { suit, rank } =
  case rank of
    Two ->
      2

    Three ->
      3

    Four ->
      4

    Five ->
      5

    Six ->
      6

    Seven ->
      7

    Eight ->
      8

    Nine ->
      9

    Ten ->
      10

    Jack ->
      10

    Queen ->
      10

    King ->
      10

    Ace ->
      11

    Joker ->
      0


shuffleCards : Model -> Model
shuffleCards model =
  let
    cardArray =
      fromList model.deck

    generator =
      shuffle cardArray

    shuffles =
      generate generator model.seed

    shuffledCards =
      toList (fst shuffles)

    newSeed =
      snd shuffles
  in
    { model | deck = shuffledCards, seed = newSeed }


view : Address Action -> Model -> Html
view address model =
  div [] []


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )
