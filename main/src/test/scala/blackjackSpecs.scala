package defsheff

import scalaz.NonEmptyList

import org.specs2.mutable.Specification

class blackjackSpecs extends Specification {

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

      //"Dealer deals themselves last" >> {
      //  val deck = (1 to 10).toList
      //  initialDeal(1)(deck) must_== ((List(List(1,3), List(2,4)), (5 to 10).toList))
      //}

      //"2 Players plus the dealer" >> {
      //  val deck = (1 to 10).toList
      //  initialDeal(2)(deck) must_== ((List(List(1,4), List(2,5), List(3,6)), (7 to 10).toList))
      //}
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

}
