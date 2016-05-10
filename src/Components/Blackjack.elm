module Components.Blackjack (..) where

import Components.Deck exposing (..)
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
  | Hit
  | Stand


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
      toLower (toString suit) ++ " unit"

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


playerHtml : Address Action -> Model -> Html
playerHtml address model =
  let
    cards =
      List.map cardToHtml model.player.hand
  in
    div
      [ class "line player" ]
      [ div
          [ class "cards" ]
          cards
      , gameButtons address model
      ]


gameTableHtml : Address Action -> Model -> Html
gameTableHtml address model =
  div
    [ id "game", class "unit r-size2of3" ]
    [ dealerHtml model
    , playerHtml address model
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


gameButtons : Address Action -> Model -> Html
gameButtons address model =
  div
    [ class "line" ]
    [ button
        [ class "unit", onClick address Hit ]
        [ text "Hit me!" ]
    , button
        [ class "unit", onClick address Stand ]
        [ text "Stand" ]
    ]


view : Address Action -> Model -> Html
view address model =
  div
    [ class "line" ]
    [ gameTableHtml address model
    , sidebarHtml address model
    , text (toString model.player)
    , text (toString model.dealer)
    ]


newGame : Model -> Model
newGame model =
  let
    newGameModel =
      { model | deck = newDeck }

    reshuffled =
      shuffleCards newGameModel

    newGameMod =
      initialDeal reshuffled
  in
    { newGameMod | gamesPlayed = model.gamesPlayed + 1 }


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


isBlackjack : List Card -> PlayerStatus
isBlackjack hand =
  if List.length hand == 2 then
    if scoreHand hand == 21 then
      Blackjack
    else
      Playing
  else
    Playing


hit : Model -> Model
hit model =
  let
    player =
      model.player

    maybeNextCard =
      List.head model.deck

    maybeDeck =
      List.tail model.deck

    newHand =
      case maybeNextCard of
        Just card ->
          card :: player.hand

        Nothing ->
          player.hand

    status =
      if (scoreHand newHand) > 21 then
        Bust
      else
        Playing

    updatedPlayer =
      { player | hand = newHand, status = status, score = scoreHand newHand }

    deck =
      List.drop 1 model.deck
  in
    { model | deck = deck, player = updatedPlayer }


stand : Model -> Model
stand model =
  let
    player =
      model.player

    updatedPlayer =
      { player | status = Standing }
  in
    { model | player = updatedPlayer }


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
      { player | hand = playerCards, score = scoreHand playerCards, status = isBlackjack playerCards }

    upDealer =
      { dealer | hand = dealerCards, score = scoreHand dealerCards, status = isBlackjack dealerCards }
  in
    { model | deck = playedDeck, player = upPlayer, dealer = upDealer }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    NewGame ->
      ( newGame model, Effects.none )

    Hit ->
      ( hit model, Effects.none )

    Stand ->
      ( stand model, Effects.none )
