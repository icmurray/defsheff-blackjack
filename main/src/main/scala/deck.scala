package defsheff

object deck {

  /* Deal `handSize` items to the given number of players.  Return the undealt items too. */
  def deal[A](numPlayers: Int, handSize: Int)(deck: List[A]): Option[(List[List[A]], List[A])] = {
    val numCards = numPlayers * handSize
    SeqUtils.partitionByDealing((numPlayers, deck.take(numCards)))
      .map(dealt => (dealt, deck.drop(numCards)))
  }
}
