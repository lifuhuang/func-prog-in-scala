package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def loop(idx: Int, until: Int, opened: Int): Boolean = {
      if (idx == until) {
        opened == 0
      } else if (chars(idx) == '(') {
        loop(idx + 1, until, opened + 1)
      } else if (chars(idx) == ')') {
        if (opened == 0) {
          false
        } else {
          loop(idx + 1, until, opened - 1)
        }
      } else {
        loop(idx + 1, until, opened)
      }
    }
    loop(0, chars.length, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until) {
        (arg1, arg2)
      } else if (chars(idx) == ')') {
        if (arg2 > 0) {
          traverse(idx + 1, until, arg1, arg2 - 1)
        } else {
          traverse(idx + 1, until, arg1 + 1, arg2)
        }
      } else if (chars(idx) == '(') {
        traverse(idx + 1, until, arg1, arg2 + 1)
      } else {
        traverse(idx + 1, until, arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (from + until) >> 1
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
        if (l._2 < r._1) {
          (l._1 + r._1 - l._2, r._2)
        } else {
          (l._1, l._2 - r._1 + r._2)
        }
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
