package defsheff

import org.specs2.mutable.Specification

class BlackJackSpecs extends Specification {

  import BlackJack._

  "BlackJack" >> {
    "Hand Scores" >> {
      "2H 2C = 4" >> { parseHandForValue("2H 2C") must_== (4) }
      "2H 2C 2D 2S = 8" >> { parseHandForValue("2H 2C 2D 2S") must_== (8) }
      "2H 2C 7S 8D = 19" >> { parseHandForValue("2H 2C 7S 8D") must_== (19) }
      "KH 5C 6S = 21" >> { parseHandForValue("KH 5C 6S") must_== (21) }
      "KH QC 3S = 23" >> { parseHandForValue("KH QC 3S") must_== (23) }
      "9H AC 3S KS = 23" >> { parseHandForValue("9H AC 3S KS") must_== (23) }
      "JH 8C 9S = 27" >> { parseHandForValue("JH 8C 9S") must_== (27) }
      "AS KS = 21" >> { parseHandForValue("AS KS") must_== (21) }
      "AS 5H = 16" >> { parseHandForValue("AS 5H") must_== (16) }
      "AS 5H 7C = 13" >> { parseHandForValue("AS 5H 7C") must_== (13) }
      "AS AC = 12" >> { parseHandForValue("AS AC") must_== (12) }
      "AS AC AD AH = 14" >> { parseHandForValue("AS AC AD AH") must_== (14) }
      "AS AC KC = 12" >> { parseHandForValue("AS AC KC") must_== (12) }
      "AS AC AH AD KC = 14" >> { parseHandForValue("AS AC AH AD KC") must_== (14) }
      "AS AC AH AD KC JH = 24" >> { parseHandForValue("AS AC AH AD KC JH") must_== (24) }
      "10H QC AS = 21" >> { parseHandForValue("10H QC AS") must_== (21) }
    }

  }

  private def parseHandForValue(s: String) = BlackJack.handValue(parseHand(s))
  private def parseHand(s: String) = s.split(" ").map(parseCard).toList
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

}
