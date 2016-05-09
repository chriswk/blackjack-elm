module Components.Blackjack (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Signal exposing (Signal, Address)
import Effects exposing (Effects)
import Array exposing (toList, fromList)
import Random.Array exposing (shuffle)
import String exposing (toLower)


type Action
  = NoOp
  | NewGame


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


type PlayerStatus
  = Playing
  | Standing
  | Bust
  | Blackjack


type alias Dealer =
  { hand : List Card
  , score : Int
  , status : PlayerStatus
  , hitLimit : Int
  }


type alias Player =
  { hand : List Card
  , score : Int
  , status : PlayerStatus
  }


type alias Model =
  { deck : Deck
  , seed : Seed
  , player : Player
  , dealer : Dealer
  , playerWins : Float
  , dealerWins : Float
  , gamesPlayed : Float
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


cardToHtml : Card -> Html
cardToHtml { suit, rank } =
  let
    suitClass =
      toLower (toString suit)

    rankClass =
      toLower (toString rank)
  in
    div
      [ class suitClass ]
      [ div [ class rankClass ] []
      ]


gameStatus : Model -> Html
gameStatus model =
  let
    playerWins =
      "Player wins: " ++ (toString model.playerWins)

    gamesPlayed =
      "Games played: " ++ (toString model.gamesPlayed)

    dealerWins =
      "Dealer wins: " ++ (toString model.dealerWins)

    winPercentage =
      if model.gamesPlayed > 0 then
        "Win percentage: " ++ (toString (model.playerWins / model.gamesPlayed)) ++ "%"
      else
        "No games played yet"
  in
    div
      [ class "line gameStatus" ]
      [ div [ class "line player" ] [ div [ class "unit" ] [ text playerWins ] ]
      , div [ class "line dealer" ] [ div [ class "unit" ] [ text dealerWins ] ]
      , div [ class "line played" ] [ div [ class "unit" ] [ text gamesPlayed ] ]
      , div [ class "line winPercentage" ] [ div [ class "unit" ] [ text winPercentage ] ]
      ]


dealerHtml : Model -> Html
dealerHtml model =
  let
    cards =
      List.map cardToHtml model.dealer.hand
  in
    div
      [ class "line dealer" ]
      cards


playerHtml : Model -> Html
playerHtml model =
  let
    cards =
      List.map cardToHtml model.player.hand
  in
    div
      [ class "line player" ]
      cards


gameTableHtml : Address Action -> Model -> Html
gameTableHtml address model =
  div
    [ id "game", class "unit r-size2of3" ]
    [ dealerHtml model
    , playerHtml model
    ]


sidebarHtml : Address Action -> Model -> Html
sidebarHtml address model =
  div
    [ id "sidebar", class "unit r-size1of3" ]
    [ button
        [ onClick address NewGame ]
        [ text "New Game" ]
    , gameStatus model
    ]


buttonHtml : Address Action -> Html
buttonHtml address =
  div
    []
    [ button
        [ onClick address NewGame ]
        [ text "New game" ]
    ]


view : Address Action -> Model -> Html
view address model =
  div
    [ class "line" ]
    [ gameTableHtml address model
    , sidebarHtml address model
    , text (toString model)
    ]


newGame : Model -> Model
newGame model =
  let
    reshuffled =
      shuffleCards model
  in
    { reshuffled | gamesPlayed = model.gamesPlayed + 1 }


cardScorer : Card -> Int -> Int
cardScorer card soFar =
  if card.rank == Ace then
    if (soFar + (valueOfCard card)) > 21 then
      soFar + 1
    else
      soFar + (valueOfCard card)
  else
    soFar + (valueOfCard card)


scoreHand : List Card -> Int
scoreHand hand =
  List.foldl cardScorer 0 hand


initialDeal : Model -> Model
initialDeal model =
  let
    playerCards =
      List.take 2 model.deck

    dealerCards =
      List.take 2 (List.drop 2 model.deck)

    playedDeck =
      List.drop 4 model.deck

    player =
      model.player

    dealer =
      model.dealer

    upPlayer =
      { player | hand = playerCards, score = scoreHand playerCards }

    upDealer =
      { dealer | hand = dealerCards, score = scoreHand dealerCards }
  in
    { model | deck = playedDeck, player = upPlayer, dealer = upDealer }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    NewGame ->
      ( newGame model, Effects.none )
