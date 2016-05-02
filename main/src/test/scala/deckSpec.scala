package defsheff

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import org.scalacheck._

import data._

class DeckSpec extends Specification with ScalaCheck {

  "standard deck" >> {
    "contains 52 unique cards" >> {
      deck.standardDeck.toSet.size must_=== (52)
    }
  }

  "deal" >> {

    case class ValidPrecondition(numHands: Int, handSize: Int, deck: Deck) {
      val cardsRequired = numHands * handSize
      def undealt = deck.drop(cardsRequired)
      def deal = defsheff.deck.deal(numHands, handSize)(deck).get
    }

    val CardGen = Gen.oneOf(deck.standardDeck)
    implicit val Arb = Arbitrary(
      for {
        numPlayers <- Gen.posNum[Int]
        handSize <- Gen.posNum[Int]
        undealt <- Gen.listOf(CardGen)
        toDeal <- Gen.listOfN(numPlayers*handSize, CardGen)
      } yield ValidPrecondition(numPlayers, handSize, toDeal ++ undealt)
    )

    "returns the undealt cards un-touched" >> prop { (test: ValidPrecondition) =>
      test.deal._2 must_=== (test.undealt)
    }
  }
}
