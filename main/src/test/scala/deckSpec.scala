package defsheff

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.scalacheck.Parameters

import org.scalacheck._

import data._

class DeckSpec extends Specification with ScalaCheck {

  implicit private val params = Parameters(minTestsOk = 2000)

  "standard deck" >> {
    "contains 52 unique cards" >> {
      deck.standardDeck.toSet.size must_=== (52)
    }
  }

  "deal" >> {
    import DealPreconditions._

    "returns the undealt cards un-touched" >> prop { (test: ValidPrecondition) =>
      test.deal._2 must_=== (test.undealt)
    }

    "returns the requested number of hands" >> prop { (test: ValidPrecondition) =>
      test.deal._1.length must_=== (test.numHands)
    }

    "invalid arguments result in None" >> prop { (invalid: InvalidPrecondition) =>
      invalid.deal must beNone
    }
  }

  object DealPreconditions {

    case class ValidPrecondition(numHands: Int, handSize: Int, deck: Deck) {
      require(numHands * handSize <= deck.length)
      val cardsRequired = numHands * handSize
      def undealt = deck.drop(cardsRequired)
      def deal = defsheff.deck.deal(numHands, handSize)(deck).get
    }

    case class InvalidPrecondition(numHands: Int, handSize: Int, deck: Deck) {
      def deal = defsheff.deck.deal(numHands, handSize)(deck)
    }

    val CardGen = Gen.oneOf(deck.standardDeck)
    val HandParamsGen = for {
      numHands <- Gen.posNum[Int]
      handSize <- Gen.posNum[Int]
    } yield (numHands, handSize)
    def DeckGen(size: Int) = Gen.listOfN(size, CardGen)

    val PlentyInTheDeck = for {
      (numHands, handSize) <- HandParamsGen
      undealt <- Gen.listOf(CardGen)
      toDeal <- DeckGen(numHands * handSize)
      deck = toDeal ++ undealt
    } yield ValidPrecondition(numHands, handSize, deck)

    val JustEnoughInTheDeck = for {
      (numHands, handSize) <- HandParamsGen
      deck <- DeckGen(numHands * handSize)
    } yield ValidPrecondition(numHands, handSize, deck)

    val DeckTooSmall = for {
      (numHands, handSize) <- HandParamsGen
      deckSize <- Gen.choose(0, (numHands * handSize) - 1)
      deck <- DeckGen(deckSize)
    } yield InvalidPrecondition(numHands, handSize, deck)

    implicit val Valid: Arbitrary[ValidPrecondition] = Arbitrary(
      Gen.oneOf(PlentyInTheDeck, JustEnoughInTheDeck))

    implicit val Invalid: Arbitrary[InvalidPrecondition] = Arbitrary(DeckTooSmall)

  }
}
