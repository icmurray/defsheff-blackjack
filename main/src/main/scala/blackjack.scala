package defsheff

import scala.io.StdIn.readLine
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

  object Outcome {
    implicit val order: Order[Outcome] = Order.order {
      case (Blackjack, Blackjack) => Ordering.EQ
      case (Blackjack, _        ) => Ordering.GT
      case (_        , Blackjack) => Ordering.LT
      case (Score(_),  Bust(_)  ) => Ordering.GT
      case (Bust(_),   Score(_) ) => Ordering.LT
      case (Bust(_),   Bust(_)  ) => Ordering.EQ
      case (Score(s1), Score(s2)) => s1 cmp s2
    }
  }

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
  sealed trait GameState {
    def table: Table

    /** Who is actively playing in this state? **/
    def active: Lens[Table,Hand]

    /** The Hand of the active player **/
    def activeHand = active.get(table)
  }
  final case class PlayerTurn(table: Table) extends GameState { val active = Table.playerL }
  final case class DealerTurn(table: Table) extends GameState { val active = Table.dealerL }

  /** Distinct from the GameState is a game with no further actions **/
  final case class GameFinished(table: Table) {
    def outcome: GameOutcome = {
      val player = handOutcome(table.player)
      val dealer = handOutcome(table.dealer)
      player cmp dealer match {
        case Ordering.GT => PlayerWins(player, dealer)
        case otherwise   => PlayerLoses(player, dealer)
      }
    }
  }

  sealed trait GameOutcome
  final case class PlayerWins(player: Outcome, dealer: Outcome) extends GameOutcome
  final case class PlayerLoses(player: Outcome, dealer: Outcome) extends GameOutcome

  /** The effect data-type the game runs in.
    * Isomorphic to Task[Option[A]], where Task captures any unpure IO, and
    * Option captures any failures (currently, the deck may run out of cards).
    */
  type Game[A] = OptionT[Task,A]

  object Game {
    def now[A](oa: Option[A]): Game[A] = OptionT(Task.now(oa))
    def delay[A](oa: Option[A]): Game[A] = OptionT(Task.delay(oa))
    def pure[A](a: A): Game[A] = a.point[Game]
  }

  /** Playing the game means just playing until the user quits **/
  case object UserQuit
  type MainGame = Game[UserQuit.type]

  /** Deal two Cards to each player, plus the dealer **/
  def blackjackDeal(numPlayers: Int) = deal(numPlayers+1, 2) _

  /** Just 1 player at a Table **/
  def initialTable(deck: Deck) = {
    blackjackDeal(1)(deck).map { r => (r: @unchecked) match {
      case (hand1 :: hand2 :: Nil, undealt) => Table(dealer=hand2, player=hand1, deck=undealt)
    }}
  }

  /** Return the best value for the given Hand **/
  def handValue(hand: Hand): Int = {

    def cardValue(c: Card) = c.rank match {
      case NC(r)     => NonEmptyList(r)
      case Ace       => NonEmptyList(1, 11)
      case otherwise => NonEmptyList(10)
    }

    hand.map(cardValue).sequence.map(_.sumr).partition(_ <= 21) match {
      case (Nil,  bust) => bust.min
      case (legal, _)   => legal.max
    }
  }

  /** Return the best Outcome for the given Hand **/
  def handOutcome(hand: Hand): Outcome = {

    def isBlackJack(hand: Hand) =
      handValue(hand) == 21 && hand.length == 2

    handValue(hand) match {
      case v if (isBlackJack(hand)) => Blackjack
      case v if (v > 21)            => Bust(v)
      case v if (v <= 21)           => Score(v)
    }
  }

  /** Inspect the given state and determine if the active player is bust **/
  def isBust(state: GameState) = {
    handOutcome(state.activeHand) match {
      case Bust(_)   => true
      case otherwise => false
    }
  }

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
      case PlayerTurn(_) => dealTo(game.active).map(PlayerTurn(_).asInstanceOf[S])
      case DealerTurn(_) => dealTo(game.active).map(DealerTurn(_).asInstanceOf[S])
    }
  }

  /** Standing just means transitioning to the next state.
    * Method over-loading. Hmm... bad practice, but useful in this case.
    */
  def stand(state: PlayerTurn): DealerTurn = DealerTurn(state.table)
  def stand(state: DealerTurn): GameFinished = GameFinished(state.table)

  /** Functions related to the UI **/
  object ui {

    def print(s: String): Game[Unit] = Game.pure(println(s))
    def printPlayerTurn(state: PlayerTurn) = print(playerTurnS(state))
    def printGameFinished(g: GameFinished)= print(gameFinishedS(g))

    val promptPlayAgain = prompt[Boolean]("Play again? [y|n]") {
      case yes if yes.toUpperCase == "YES" => true
      case y   if y.toUpperCase   == "Y"   => true
      case no  if no.toUpperCase  == "NO"  => false
      case n   if n.toUpperCase   == "N"   => false
    }.liftM[OptionT]

    val promptHitOrStand = prompt[Decision]("[H]it or [S]tand?") {
      case h if h.toUpperCase == "H"   => Hit
      case s if s.toUpperCase == "S" => Stand
    }.liftM[OptionT]

    def prompt[A](question: String)(pf: PartialFunction[String,A]): Task[A] = {
      Task.delay(readLine(question + " ")).flatMap {
        case s if pf.isDefinedAt(s) => Task.now(pf(s))
        case otherwise              => prompt[A](question)(pf)
      }
    }

    private def fullHandS(hand: Hand): String = {
      val handS = hand.mkString(" ")
      val scoreS = s"[${handValue(hand)}]"
      handS.padTo(30, ' ') ++ scoreS
    }

    private def hiddenHandS(hand: Hand): String = {
      val redact = (c: Card) => " ??"
      (hand.head.shows :: hand.tail.map(redact).toList).mkString(" ")
    }

    private def playerTurnS(state: PlayerTurn): String = {
      s"""You:    ${fullHandS(state.table.player)}
         |Dealer: ${hiddenHandS(state.table.dealer)}""".stripMargin
    }

    private def outcomeS(o: GameOutcome) = o match {
      case PlayerWins(Blackjack,  _)         => "Blackjack!  You Win!!!"
      case PlayerWins(_,          _)         => "You Win!"
      case PlayerLoses(Blackjack, Blackjack) => "So close, but you lose."
      case PlayerLoses(Bust(_),   _)         => "Bust! Loser."
      case PlayerLoses(_,         _)         => "You lose."
    }

    private def gameFinishedS(g: GameFinished): String = {
      s"""--------------------------------------------------
         |You:    ${fullHandS(g.table.player)}
         |Dealer: ${fullHandS(g.table.dealer)}
         |
         |${outcomeS(g.outcome)}
         |--------------------------------------------------""".stripMargin
    }

  }

  /** Functions related to the game control flow **/
  object control {

    /** Keep playing until the Deck is empty **/
    def mainGame(deck: Deck): MainGame = {
      val quit: MainGame = Game.pure(UserQuit)
      for {
        nextDeck <- singleGame(deck)
        playAgain <- ui.promptPlayAgain
        q <- if (playAgain) mainGame(nextDeck) else quit
      } yield q
    }

    /** A single deal and playing of the game.  Returns the remainder of
      *  the unused Deck.
      */
    def singleGame(deck: Deck): Game[Deck] = {
      for {
        table <- Game.now(initialTable(deck))
        initialState = PlayerTurn(table)
        dealerState <- playerTurn(initialState)
        finalState <- dealerTurn(dealerState)
        _ <- ui.printGameFinished(finalState)
      } yield finalState.table.deck
    }

    /** Repeatedly applies the given strategy to the given GameState, until
      * either the strategy decides to Stand, or the Hand goes bust.  Doesn't
      * transition the GameState.
      */
    private def turn[S <: GameState](strategy: S => Game[Decision]): S => Game[S] = {
      val pred = (s: S) =>
        if (isBust(s)) Game.pure(false) else strategy(s).map(_ == Hit)

      stepWhile[Game,S](pred, s => Game.now(hit(s))) _
    }

    /** Prompt the user for the player's decision **/
    private def playerStrategy(state: PlayerTurn): Game[Decision] = {
      for {
        _ <- ui.printPlayerTurn(state)
        decision <- ui.promptHitOrStand
      } yield decision
    }

    /** House rules: hit until 17 is reached **/
    private def dealerStrategy(state: DealerTurn): Game[Decision] = Game.pure(
      if (handValue(state.table.dealer) < 17) Hit else Stand
    )

    private def playerTurn(state: PlayerTurn) = turn(playerStrategy _)(state).map(stand)
    private def dealerTurn(state: DealerTurn) = turn(dealerStrategy _)(state).map(stand)

    /** TODO: move to general control module. **/
    private def stepWhile[M[_]:Monad, A](cond: A => M[Boolean], step: A => M[A])(a: A): M[A] = {
      for {
        p <- cond(a)
        lastResult <- if (p) step(a).flatMap(stepWhile(cond, step)) else a.pure[M]
      } yield lastResult
    }

  }

  def main(args: Array[String]): Unit = {
    import deck._
    val main = shuffleDeck(standardDeck).liftM[OptionT].flatMap(control.mainGame).run
    main.unsafePerformSync match { 
      case None           => println("Oh dear, we ran out of cards")
      case Some(UserQuit) => println("Bye then.")
    }
  }

  private implicit class NelOps[A](nel: NonEmptyList[A]) {
    def partition(pred: A => Boolean): (List[A], List[A]) = nel.list.toList.partition(pred)
    def mkString(sep: String)(implicit ev: Show[A]): String = nel.map(_.shows).list.toList.mkString(sep)
  }

}
