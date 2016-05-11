port module Main exposing (..)

import Components.Blackjack exposing (..)
import Components.Deck exposing (newDeck)
import Random exposing (Seed)
import Task
import Html.App as Html
import Random exposing (initialSeed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)


initialModel : Model
initialModel =
    { deck = []
    , seed = initialSeed 5115151
    , playerWins = 0
    , dealerWins = 0
    , draws = 0
    , gamesPlayed = 0
    , player = Player [] 0 Playing
    , dealer = Dealer [] 0 Playing 17
    , gameStatus = InProgress
    }


timeToSeed : Time -> Seed
timeToSeed t =
    initialSeed (round (Time.inMilliseconds t))


getStartTimeAsSeed : Seed -> Cmd Seed
getStartTimeAsSeed initialSeed =
    Task.perform (\t -> initialSeed) timeToSeed Time.now


init : ( Model, Cmd Msg )
init =
    let
        deck =
            newDeck

        seed =
            getStartTimeAsSeed initialModel.seed

        m =
            { initialModel | deck = deck }

        model =
            initialDeal (shuffleCards m)
    in
        ( model, Cmd.none )


main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


view : Model -> Html Msg
view model =
    let
        dealerStatus =
            "Dealer status: " ++ (toString model.dealer)

        playerStatus =
            "Player status: " ++ (toString model.player)

        gameState =
            "Current game state: " ++ (toString model.gameStatus)
    in
        div [ class "line" ]
            [ gameTableHtml model
            , sidebarHtml model
            , div [ class "unit" ] [ text playerStatus ]
            , div [ class "unit" ] [ text dealerStatus ]
            , div [ class "unit" ] [ text gameState ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            ( newGame model, Cmd.none )

        Hit ->
            ( hit model, Cmd.none )

        Stand ->
            ( stand model, Cmd.none )

        NewDeck deck ->
            ( { model | deck = deck }, Cmd.none )
