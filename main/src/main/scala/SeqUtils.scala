package defsheff

object SeqUtils {

  /* Partition the given items as if they were being dealt into the
   * given number of bins.  This function is only defined if the items
   * can be split into even-sized bins.
   */
  def partitionByDealingUnsafe[A]: PartialFunction[(Int, Seq[A]), List[List[A]]] = {
    case (numBins, items) if (items.isEmpty) =>
      List.fill(numBins)(Nil)
    case (numBins, items) if (numBins > 0 && items.length % numBins == 0) =>
      items.grouped(numBins).toList.transpose
  }

  /* Safe version of `partitionByDealingUnsafe` which wraps the result
   *  in an `Option`.
   */
  def partitionByDealing[A] = partitionByDealingUnsafe[A].lift
}
