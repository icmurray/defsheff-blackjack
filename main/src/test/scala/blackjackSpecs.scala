package defsheff

import scalaz.NonEmptyList

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.scalacheck.Parameters

import org.scalacheck._

class blackjackSpecs extends Specification with ScalaCheck {

  import blackjack._
  import deck.standardDeck
  import data._

  "BlackJack" >> {
    "Hand Scores" >> {
      "2H 2C = 4" >> { parseHandForValue("2H 2C") must_== Score(4) }
      "2H 2C 2D 2S = 8" >> { parseHandForValue("2H 2C 2D 2S") must_== Score(8) }
      "2H 2C 7S 8D = 19" >> { parseHandForValue("2H 2C 7S 8D") must_== Score(19) }
      "KH 5C 6S = 21" >> { parseHandForValue("KH 5C 6S") must_== Score(21) }
      "KH QC 3S = 23" >> { parseHandForValue("KH QC 3S") must_== Bust(23) }
      "9H AC 3S KS = 23" >> { parseHandForValue("9H AC 3S KS") must_== Bust(23) }
      "JH 8C 9S = 27" >> { parseHandForValue("JH 8C 9S") must_== Bust(27) }
      "AS KS = 21" >> { parseHandForValue("AS KS") must_== Blackjack }
      "AS 5H = 16" >> { parseHandForValue("AS 5H") must_== Score(16) }
      "AS 5H 7C = 13" >> { parseHandForValue("AS 5H 7C") must_== Score(13) }
      "AS AC = 12" >> { parseHandForValue("AS AC") must_== Score(12) }
      "AS AC AD AH = 14" >> { parseHandForValue("AS AC AD AH") must_== Score(14) }
      "AS AC KC = 12" >> { parseHandForValue("AS AC KC") must_== Score(12) }
      "AS AC AH AD KC = 14" >> { parseHandForValue("AS AC AH AD KC") must_== Score(14) }
      "AS AC AH AD KC JH = 24" >> { parseHandForValue("AS AC AH AD KC JH") must_== Bust(24) }
      "10H QC AS = 21" >> { parseHandForValue("10H QC AS") must_== Score(21) }
    }

    "Dealing Blackjack" >> {
      "Lone dealer" >> {
        val deck = standardDeck
        blackjackDeal(0)(deck) must beSome (be_==((List(deck.take(2).toNelUnsafe), deck.drop(2))))
      }
    }

    "hit" >> {

      "Non-empty deck" >> {

        "deck has card removed from top" >> prop { (state: GameState) =>
          (state.table.deck.nonEmpty) ==> {
            blackjack.hit(state)
              .map(_.deck) must beSome(be_==(state.deck.tail))
          }
        }

        "PlayerTurn" >> {
          "player gets a new card from the top of the deck" >> prop { (state: PlayerTurn) =>
            (state.table.deck.nonEmpty) ==> {
                val expectedHand = state.topOfDeckUnsafe :: state.playerHand
                blackjack.hit(state)
                  .map(_.playerHand) must beSome (contain(exactly(expectedHand:_*)))
            }
          }

          "dealer hand unchanged" >> prop { (state: PlayerTurn) =>
            (state.table.deck.nonEmpty) ==> {
                blackjack.hit(state)
                  .map(_.dealerHand) must beSome (be_==(state.dealerHand))
            }
          }
        }

        "DealerTurn" >> {
          "dealer gets a new card from the top of the deck" >> prop { (state: DealerTurn) =>
            (state.table.deck.nonEmpty) ==> {
                val expectedHand = state.topOfDeckUnsafe :: state.dealerHand
                blackjack.hit(state)
                  .map(_.dealerHand) must beSome (contain(exactly(expectedHand:_*)))
            }
          }

          "player hand unchanged" >> prop { (state: DealerTurn) =>
            (state.table.deck.nonEmpty) ==> {
                blackjack.hit(state)
                  .map(_.playerHand) must beSome (be_==(state.playerHand))
            }
          }
        }

      }


    }

  }

  private def parseHandForValue(s: String) = blackjack.handOutcome(parseHand(s))
  private def parseHand(s: String) = s.split(" ").map(parseCard).toList.toNelUnsafe
  private def parseCard(s: String) = {
    val CardMatch = """(\d+|[AJQK])([HCDS])""" .r
    s match {
      case CardMatch("A", suit) => Card(Ace,   parseSuit(suit))
      case CardMatch("J", suit) => Card(Jack,  parseSuit(suit))
      case CardMatch("Q", suit) => Card(Queen, parseSuit(suit))
      case CardMatch("K", suit) => Card(King,  parseSuit(suit))
      case CardMatch(num, suit) => Card(NC(Integer.parseInt(num)), parseSuit(suit))
    }
  }

  private def parseSuit(s: String) = s match {
    case "C" => Clubs
    case "D" => Diamonds
    case "H" => Hearts
    case "S" => Spades
  }

  private implicit class ListOps[A](l: List[A]) {
    def toNelUnsafe: NonEmptyList[A] = l match {
      case head :: tail => NonEmptyList(head, tail:_*)
    }
  }

  val CardGen = Gen.oneOf(deck.standardDeck)
  val HandGen = for {
    card <- CardGen
    cards <- Gen.listOf(CardGen)
  } yield NonEmptyList(card, cards:_*)

  val TableGen = for {
    dealer <- HandGen
    player <- HandGen
    deck   <- Gen.listOf(CardGen)
  } yield Table(dealer=dealer, player=player, deck=deck)

  implicit val PlayerTurnArb = Arbitrary(TableGen.map(PlayerTurn.apply _))
  implicit val DealerTurnArb = Arbitrary(TableGen.map(DealerTurn.apply _))
  implicit val GameStateArb: Arbitrary[GameState]  = Arbitrary(Gen.oneOf(PlayerTurnArb.arbitrary, DealerTurnArb.arbitrary))

  implicit class GameStateOps(state: GameState) {
    def playerHand = state.table.player.list.toList
    def dealerHand = state.table.dealer.list.toList
    def deck = state.table.deck
    def topOfDeckUnsafe = state.table.deck.head
  }

}
