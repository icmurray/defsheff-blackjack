package defsheff

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import org.scalacheck._

class DeckSpec extends Specification with ScalaCheck {

  "deal" >> {

    case class ValidPrecondition(numPlayers: Int, handSize: Int, items: List[String]) {
      val cardsRequired = numPlayers * handSize
      def undealt = items.drop(cardsRequired)
      def deal = deck.deal(numPlayers, handSize)(items).get
    }

    implicit val Arb = Arbitrary(
      for {
        numPlayers <- Gen.posNum[Int]
        handSize <- Gen.posNum[Int]
        undealt <- Gen.listOf(Gen.alphaStr)
        toDeal <- Gen.listOfN(numPlayers*handSize, Gen.alphaStr)
      } yield ValidPrecondition(numPlayers, handSize, toDeal ++ undealt)
    )

    "returns the undealt cards un-touched" >> prop { (test: ValidPrecondition) =>
      test.deal._2 must_=== (test.undealt)
    }
  }
}
