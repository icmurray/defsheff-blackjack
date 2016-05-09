package defsheff

import scala.language.higherKinds

import scalaz.{Lens => _, _} // Hide scalaz.Lens
import scalaz.concurrent._
import Scalaz._

import monocle._
import monocle.macros._

import data._
import deck.{deal, dealOne}

object blackjack {

  /** Outcome of a single Hand **/
  sealed trait Outcome { def value: Int }
  final case class Score(value: Int) extends Outcome
  final case class Bust(value: Int) extends Outcome
  final case object Blackjack  extends Outcome { def value = 21 }

  /** A player's decision **/
  sealed trait Decision
  case object Hit extends Decision
  case object Stand extends Decision

  /** State of the hands in play, and the deck **/
  final case class Table(dealer: Hand, player: Hand, deck: Deck)
  object Table {
    val deckL   = GenLens[Table](_.deck)
    val playerL = GenLens[Table](_.player)
    val dealerL = GenLens[Table](_.dealer)
  }

  /** GameState tracks the Table, and who is currently active **/
  sealed trait GameState { def table: Table }
  final case class PlayerTurn(table: Table) extends GameState
  final case class DealerTurn(table: Table) extends GameState

  /** Distinct from the GameState is a game with no further actions **/
  final case class GameFinished(table: Table)

  /** The effect data-type the game runs in.
    * Isomorphic to Task[Option[A]], where Task captures any unpure IO, and
    * Option captures any failures (eg. - deck runs out of cards).
    */
  type Game[A] = OptionT[Task,A]

  /** Deal a new card to the active player.
    * Un-decided about the merits of defining this function with these type bounds.
    * Potentially unsafe if I mess up the function definition, but I don't know how
    * to implement it without the typecasts.
    */
  def hit[S <: GameState](game: S): Option[S] = {

    def dealTo(activeHand: Lens[Table,Hand]): Option[Table] = {
      val updateHandAndDeck = ((card: Card, deck: Deck) =>
        activeHand.modify(card <:: _) compose Table.deckL.set(deck)).tupled
      dealOne(game.table.deck).map(updateHandAndDeck(_)(game.table))
    }

    game match {
      case PlayerTurn(_) => dealTo(Table.playerL).map(PlayerTurn(_).asInstanceOf[S])
      case DealerTurn(_) => dealTo(Table.dealerL).map(DealerTurn.apply(_).asInstanceOf[S])
    }
  }

  /** Standing just means transitioning to the next state.
    * Method over-loading. Hmm... bad practice, but useful in this case.
    */
  def stand(state: PlayerTurn): DealerTurn = DealerTurn(state.table)
  def stand(state: DealerTurn): GameFinished = GameFinished(state.table)

  val promptHitOrStand: Task[Decision] = {
    import scala.io.StdIn.readLine
    Task.delay(readLine("[H]it or [S]tand? ").toUpperCase).flatMap {
      case "H"       => Task.now(Hit)
      case "S"       => Task.now(Stand)
      case otherwise => promptHitOrStand
    }
  }

  def showState(state: GameState): String = state match {
    case PlayerTurn(Table(dealer, player, deck)) =>
      s"You: $player\t Dealer: <redacted>"
    case DealerTurn(Table(dealer, player, deck)) =>
      s"You: $player\t Dealer: $dealer"
  }

  def playerTurn(state: PlayerTurn): Game[DealerTurn] = {

    val printState: Game[Unit] = OptionT.some(println(showState(state)))
    val handleStand: Game[DealerTurn] = OptionT.some(stand(state))
    val handleHit: Game[DealerTurn] = {
      hit(state) match {
        case None => OptionT.none
        case Some(PlayerTurn(table)) if (handValue(table.player) > 21) =>
          OptionT.some(DealerTurn(table))
        case Some(nextState@PlayerTurn(_)) =>
          playerTurn(nextState)
      }
    }

    for {
      _ <- printState
      decision <- promptHitOrStand.liftM[OptionT]
      nextState <- decision match {
        case Hit =>   handleHit
        case Stand => handleStand
      }
    } yield nextState
  }

  def dealerTurn(state: DealerTurn): Option[GameFinished] = {
    if (handValue(state.table.dealer) < 17) {
      hit(state).flatMap(dealerTurn)
    } else {
      Some(stand(state))
    }
  }

  def singleGame(deck: Deck): Game[Deck] = {

    def printGameFinished(state: GameFinished): Game[Unit] = OptionT.some {
      println(s"You: ${state.table.player} [${handValue(state.table.player)}]\n" ++
        s"Dealer: ${state.table.dealer} [${handValue(state.table.dealer)}]")
    }

    for {
      table <- OptionT(Task.now(initialTable(deck)))
      initialState = PlayerTurn(table)
      dealerState <- playerTurn(initialState)
      finalState <- OptionT(Task.now(dealerTurn(dealerState)))
      _ <- printGameFinished(finalState)
    } yield finalState.table.deck
  }

  def untilTheDeckIsEmpty(deck: Deck): Game[Unit] = {
    for {
      nextDeck <-singleGame(deck)
      _ <- untilTheDeckIsEmpty(nextDeck)
    } yield ()
  }

  def blackjackDeal(numPlayers: Int) = deal(numPlayers+1, 2) _

  def initialTable(deck: Deck) = {
    blackjackDeal(1)(deck).map { r => (r: @unchecked) match {
      case (hand1 :: hand2 :: Nil, undealt) => Table(dealer=hand2, player=hand1, deck=undealt)
    }}
  }

  def cardValue(c: Card) = c.rank match {
    case NC(r)     => NonEmptyList(r)
    case Ace       => NonEmptyList(1, 11)
    case otherwise => NonEmptyList(10)
  }

  def handValue(hand: Hand): Int = {
    hand.map(cardValue).sequence.map(_.sumr).partition(_ <= 21) match {
      case (Nil,  bust) => bust.min
      case (legal, _)   => legal.max
    }
  }

  def handOutcome(hand: Hand): Outcome = {
    handValue(hand) match {
      case v if (isBlackJack(hand)) => Blackjack
      case v if (v > 21)  => Bust(v)
      case v if (v <= 21) => Score(v)
    }
  }

  def isBlackJack(hand: Hand) = handValue(hand) == 21 && hand.length == 2

  private implicit class NelOps[A](nel: NonEmptyList[A]) {
    def partition(pred: A => Boolean): (List[A], List[A]) = nel.list.toList.partition(pred)
  }

  def main(args: Array[String]): Unit = {
    import deck._
    val main = shuffleDeck(standardDeck).liftM[OptionT].flatMap(untilTheDeckIsEmpty).run
    val outcome = main.unsafePerformSync
  }
}
