module Components.Blackjack exposing (..)

import Components.Deck exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed)
import Array exposing (toList, fromList)
import String exposing (toLower)


type Msg
    = NoOp
    | NewGame
    | Hit
    | Stand
    | NewDeck Deck


type GameStatus
    = InProgress
    | Draw
    | PlayerWin
    | DealerWin


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
    , draws : Float
    , gamesPlayed : Float
    , gameStatus : GameStatus
    }


shuffleCards : Model -> Model
shuffleCards model =
    let
        cardArray =
            fromList model.deck

        generator =
            shuffle cardArray

        shuffles =
            Random.step generator model.seed

        shuffledCards =
            toList (fst shuffles)

        newSeed =
            snd shuffles
    in
        { model | deck = shuffledCards, seed = newSeed }


cardToHtml : Card -> Html Msg
cardToHtml { suit, rank } =
    let
        suitClass =
            toLower (toString suit) ++ " unit"

        rankClass =
            toLower (toString rank)
    in
        div [ class suitClass ]
            [ div [ class rankClass ] []
            ]


gameStatus : Model -> Html Msg
gameStatus model =
    let
        playerWins =
            "Player wins: " ++ (toString model.playerWins)

        gamesPlayed =
            "Games played: " ++ (toString model.gamesPlayed)

        dealerWins =
            "Dealer wins: " ++ (toString model.dealerWins)

        draws =
            "Draws: " ++ (toString model.draws)

        winPercentage =
            if model.gamesPlayed > 0 then
                "Win percentage: " ++ (toString ((model.playerWins / model.gamesPlayed) * 100)) ++ "%"
            else
                "No games played yet"
    in
        div [ class "line gameStatus" ]
            [ div [ class "line player" ] [ div [ class "unit" ] [ text playerWins ] ]
            , div [ class "line dealer" ] [ div [ class "unit" ] [ text dealerWins ] ]
            , div [ class "line draws" ] [ div [ class "unit" ] [ text draws ] ]
            , div [ class "line played" ] [ div [ class "unit" ] [ text gamesPlayed ] ]
            , div [ class "line winPercentage" ] [ div [ class "unit" ] [ text winPercentage ] ]
            ]


dealerHtml : Model -> Html Msg
dealerHtml model =
    let
        cards =
            List.map cardToHtml model.dealer.hand
    in
        div [ class "line dealer" ]
            cards


playerHtml : Model -> Html Msg
playerHtml model =
    let
        cards =
            List.map cardToHtml model.player.hand
    in
        div [ class "line player" ]
            [ div [ class "cards" ]
                cards
            , gameButtons model
            ]


gameTableHtml : Model -> Html Msg
gameTableHtml model =
    div [ id "game", class "unit r-size2of3" ]
        [ dealerHtml model
        , playerHtml model
        ]


sidebarHtml : Model -> Html Msg
sidebarHtml model =
    div [ id "sidebar", class "unit r-size1of3" ]
        [ button [ onClick NewGame ]
            [ text "New Game" ]
        , gameStatus model
        ]


gameButtons : Model -> Html Msg
gameButtons model =
    let
        hitButton =
            if model.player.status == Playing && model.gameStatus == InProgress then
                button [ class "unit", onClick Hit ] [ text "Hit me!" ]
            else
                button [ class "unit", onClick NewGame ] [ text "Start a new game" ]

        standButton =
            if model.player.status == Playing && model.gameStatus == InProgress then
                button [ class "unit", onClick Stand ]
                    [ text "Stand" ]
            else
                button [ class "unit", disabled True ]
                    [ text "Stand" ]
    in
        div [ class "line" ]
            [ hitButton
            , standButton
            ]


newGame : Model -> Model
newGame model =
    let
        dealerWins =
            if model.gameStatus == DealerWin then
                model.dealerWins + 1
            else
                model.dealerWins

        playerWins =
            if model.gameStatus == PlayerWin then
                model.playerWins + 1
            else
                model.playerWins

        draws =
            if model.gameStatus == Draw then
                model.draws + 1
            else
                model.draws

        gamesPlayed =
            if model.gameStatus == InProgress then
                model.gamesPlayed
            else
                model.gamesPlayed + 1

        newGameModel =
            { model | deck = newDeck, dealerWins = dealerWins, playerWins = playerWins, draws = draws }

        reshuffled =
            shuffleCards newGameModel

        newGameMod =
            initialDeal reshuffled
    in
        { newGameMod | gamesPlayed = model.gamesPlayed + 1, gameStatus = InProgress }


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
    let
        sortedHand =
            List.sortBy valueOfCard hand
    in
        List.foldl cardScorer 0 sortedHand


isBlackjack : List Card -> Bool
isBlackjack hand =
    if (List.length hand == 2) && (scoreHand hand == 21) then
        True
    else
        False


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
        { model
            | deck = deck
            , player = updatedPlayer
            , gameStatus =
                if updatedPlayer.status == Bust then
                    DealerWin
                else
                    InProgress
        }


drawWhile : List Card -> List Card -> Int -> ( List Card, List Card )
drawWhile hand deck limit =
    let
        maybeNextCard =
            List.head deck

        maybeRestOfDeck =
            List.tail deck

        newHand =
            case maybeNextCard of
                Just card ->
                    card :: hand

                Nothing ->
                    hand

        restOfDeck =
            case maybeRestOfDeck of
                Just deck ->
                    deck

                Nothing ->
                    []
    in
        if scoreHand hand >= limit then
            ( hand, deck )
        else
            drawWhile newHand restOfDeck limit


playDealer : Model -> Model
playDealer model =
    let
        dealer =
            model.dealer

        draws =
            drawWhile dealer.hand model.deck dealer.hitLimit

        dealerHand =
            (fst draws)

        remainderOfDeck =
            (snd draws)

        score =
            scoreHand dealerHand

        status =
            if score > 21 then
                Bust
            else
                Standing

        updatedDealer =
            { dealer | hand = dealerHand, status = status, score = score }
    in
        { model | dealer = updatedDealer, deck = remainderOfDeck }


decideWinner : Model -> Model
decideWinner model =
    let
        player =
            model.player

        dealer =
            model.dealer

        gameStatus =
            if player.status == Bust then
                DealerWin
            else if dealer.status == Bust then
                PlayerWin
            else if player.status == Standing && dealer.status == Standing then
                if player.score > dealer.score then
                    PlayerWin
                else if dealer.score > player.score then
                    DealerWin
                else
                    Draw
            else
                model.gameStatus
    in
        { model | gameStatus = gameStatus }


stand : Model -> Model
stand model =
    let
        player =
            model.player

        updatedPlayer =
            { player | status = Standing }

        updateModelWithPlayer =
            { model | player = updatedPlayer }

        modelWithPlayedDealer =
            playDealer updateModelWithPlayer

        modelWithGameDecision =
            decideWinner modelWithPlayedDealer
    in
        modelWithGameDecision


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

        playerBlackjack =
            isBlackjack playerCards

        dealerBlackjack =
            isBlackjack dealerCards

        gameStatus =
            if playerBlackjack then
                if dealerBlackjack then
                    Draw
                else
                    PlayerWin
            else if dealerBlackjack then
                DealerWin
            else
                InProgress

        upPlayer =
            { player
                | hand = playerCards
                , score = scoreHand playerCards
                , status =
                    if playerBlackjack then
                        Blackjack
                    else
                        Playing
            }

        upDealer =
            { dealer
                | hand = dealerCards
                , score = scoreHand dealerCards
                , status =
                    if dealerBlackjack then
                        Blackjack
                    else
                        Playing
            }
    in
        { model | deck = playedDeck, player = upPlayer, dealer = upDealer, gameStatus = gameStatus }
