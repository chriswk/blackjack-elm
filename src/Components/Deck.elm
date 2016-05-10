module Components.Deck (..) where


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
