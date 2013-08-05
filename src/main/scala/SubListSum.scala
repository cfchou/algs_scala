/**
 * User: Chifeng.Chou
 * Date: 03/08/13
 * Time: 20:37
 */
object SubListSum {
  /*
  test whether some elements of @nums with sum equals @sum
  @nums contains only positive elements
   */
  def subListSum(nums: IndexedSeq[Int], sum: Int): Boolean = {
    /*
    x1, x2, ..., xi, ...
    a1, a2, ..., ai, ... : 0 or 1

    Would a1x1 + a2x2 + ... + aixi == s ?

    P(s, i): Boolean = P(s - wi, i - 1) || P(s, i - 1)
     */

    val tbl = Array.ofDim[Boolean](sum + 1, nums.size + 1)
    for (i <- 0 to nums.size) {
      tbl(0)(i) = true
    }
    for (w <- 1 to sum) {
      tbl(w)(0) = false
    }

    for (
      w <- (1 to sum);
      i <- (1 to nums.size)
    ) {
      val t = w - nums(i - 1)
      val alt = if (t < 0) false else tbl(t)(i - 1)
      tbl(w)(i) = alt || tbl(w)(i - 1)
    }

    tbl(sum)(nums.size)
  }

  def subListSumTbl(nums: IndexedSeq[Int], sum: Int): Array[Array[Boolean]] = {
    /*
    x1, x2, ..., xi, ...
    a1, a2, ..., ai, ... : 0 or 1

    Would a1x1 + a2x2 + ... + aixi == s ?

    P(s, i): Boolean = P(s - wi, i - 1) || P(s, i - 1)
     */

    val tbl = Array.ofDim[Boolean](sum + 1, nums.size + 1)
    for (i <- 0 to nums.size) {
      tbl(0)(i) = true
    }
    for (w <- 1 to sum) {
      tbl(w)(0) = false
    }

    for (
      w <- (1 to sum);
      i <- (1 to nums.size)
    ) {
      val t = w - nums(i - 1)
      val alt = if (t < 0) false else tbl(t)(i - 1)
      tbl(w)(i) = alt || tbl(w)(i - 1)
    }
    tbl
  }

  def subListSumZero(nums: IndexedSeq[Int]): Boolean = {

    val (pns, nns) = nums.partition(_ >= 0)
    val pns2 = nns.map(_ * -1)
    val psum = pns.sum
    val psum2 = pns2.sum

    if ((psum == 0 && psum2 != 0) || (psum != 0 && psum2 == 0)) false
    else if (psum == 0 && psum2 == 0) true
    else {
      val tbl = subListSumTbl(pns, psum)
      val tbl2 = subListSumTbl(pns2, psum2)

      def satisfy(k: Int): Boolean = {
        if (k == 0) false
        else if (tbl(k)(pns.size) && tbl2(k)(pns2.size)) true
        else satisfy(k - 1)
      }

      satisfy(math.min(psum, psum2))
    }
  }

  /*
  def subListSumZero(nums: IndexedSeq[Int]): Boolean = {

    val (pns, nns) = nums.partition(_ >= 0)
    val pns2 = nns.map(_ * -1)

    def satisfy(k: Int): Boolean = {
      if (k == 0) false
      else if (subListSum(pns, k) && subListSum(pns2, k)) true
      else satisfy(k - 1)
    }

    val psum = pns.sum
    val nsum = pns2.sum
    if ((psum == 0 && nsum != 0) || (psum != 0 && nsum == 0))
      false
    else {
      val maxCap = math.min(pns.sum, pns2.sum)
      if (maxCap == 0) true
      else satisfy(maxCap)
    }
  }
  */
}
