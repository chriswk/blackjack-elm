module Components.Deck exposing (..)

import Array exposing (Array)
import Random exposing (Generator, Seed)
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
  in
    List.concatMap rankForSuit suits


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

{-| Create a generator that always returns the same value.
-}
constant : a -> Generator a
constant value =
  Random.map (\_ -> value) Random.bool



{-| Sample without replacement: produce a randomly selected element of the
array, and the array with that element omitted (shifting all later elements
down). -}
choose : Array a -> Generator (Maybe a, Array a)
choose arr = if Array.isEmpty arr then constant (Nothing, arr) else
  let lastIndex = Array.length arr - 1
      front i = Array.slice 0 i arr
      back i = if i == lastIndex -- workaround for #1
               then Array.empty
               else Array.slice (i+1) (lastIndex+1) arr
      gen = Random.int 0 lastIndex
      in Random.map (\index ->
        (Array.get index arr, Array.append (front index) (back index)))
        gen

{-| Shuffle the array using the Fisher-Yates algorithm. Takes O(_n_ log _n_)
time and O(_n_) additional space. -}
shuffle : Array a -> Generator (Array a)
shuffle arr = if Array.isEmpty arr then constant arr else
  let --helper : (List a, Array a) -> Generator (List a, Array a)
      helper (done, remaining) =
        choose remaining `Random.andThen` (\(m_val, shorter) ->
          case m_val of
            Nothing -> constant (done, shorter)
            Just val -> helper (val::done, shorter))
  in Random.map (fst>>Array.fromList) (helper ([], arr))
