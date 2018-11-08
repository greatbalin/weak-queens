import com.typesafe.scalalogging.StrictLogging

/**
  * This is one of possible solutions for
  * <a href="https://projecteuler.net/problem=534">https://projecteuler.net/problem=534</a>
  */
object WeakQueens extends StrictLogging {

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      logger.error("Please, provide chess board scale as a first argument for application. Example: 8")
      System.exit(-1)
    }

    val n = args(0).toInt

    logger.info(s"Initial N=$n")

    logger.info(s"Start...")
    val all = (0 until (n - 1)).par.map { w =>
      val p = process(n, n - 1 - w)
      logger.info(s"permutations for w=$w : $p")
      p
    }.sum

    // Small heuristic:
    // for W=N-1 number of all permutations is equal to N in power of N
    val last = Math.pow(n, n).toLong
    logger.info(s"permutations for w=${n - 1} : $last")

    logger.info(s"All permutations for N=$n: ${all + last}")
  }

  def fits(q: Array[Int], w: Int, n: Int): Boolean =
    !(0 until n).exists { i =>
      (n - i <= w) &&
      (q(i) == q(n) ||
      (q(i) - q(n) == n - i) ||
      (q(n) - q(i) == n - i))
    }

  def printChessBoard(q: Array[Int]): String =
    q.indices
      .map(i => "* " * q(i) + "X " + "* " * (q.length - q(i) - 1))
      .mkString("\n")

  def process(n: Int, w: Int): Long = {
    val q = new Array[Int](n)
    process(q, w, 0)
  }

  def process(q: Array[Int], w: Int, k: Int): Long = {
    val n = q.length
    if (k == n) {
      logger.trace("permutation\n{}", printChessBoard(q))
      1L
    } else {
      (0L until n.toLong).fold(0L) { (acc, i) =>
        q(k) = i.toInt
        if (fits(q, w, k)) acc + process(q, w, k + 1) else acc
      }
    }
  }

}
