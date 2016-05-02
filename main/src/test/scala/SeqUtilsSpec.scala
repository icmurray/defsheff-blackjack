package defsheff

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import org.scalacheck._

class SeqUtilsSpec extends Specification with ScalaCheck {

  "partitionByDealingUnsafe[A]" >> {

    case class ValidPrecondition[A](numBins: Int, items: List[A]) {
      def partition = SeqUtils.partitionByDealingUnsafe((numBins, items))
      def binSize = items.length / numBins
    }

    implicit val Arb = Arbitrary(
      for {
        numBins <- Gen.posNum[Int]
        partitionSize <- Gen.oneOf(Gen.const(0), Gen.posNum[Int])
        items <- Gen.listOfN(numBins*partitionSize, Gen.alphaStr)
      } yield ValidPrecondition(numBins, items)
    )

    type Test = ValidPrecondition[String]

    "flattened, should be a permutation of the original list" >> prop { (test: Test) =>
      test.partition.flatten.toSet must_=== (test.items.toSet)
    }

    "should always contain the given number of bins" >> prop { (test: Test) =>
      test.partition.length must_=== (test.numBins)
    }

    "each bin should be the same size" >> prop { (test: Test) =>
      test.partition.map(_.length) must contain(be_==(test.binSize)).foreach
    }

    "can be undone by re-dealing to `binSize` bins" >> prop { (test: Test) =>
      val dealt = test.partition.flatten
      SeqUtils.partitionByDealingUnsafe((test.binSize, dealt)).flatten must_=== (test.items)
    }

  }

}
