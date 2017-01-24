package org.calderoni.josephus

import scala.annotation.tailrec

/**
  * Created by max on 1/22/17.
  */
object Josephus {
  val LARGE_N = 536870912

  def run(n: Int, k: Int) = {
    require(n>0, "number of players should be strictly positive")
    require(k>0, "skip should be strictly positive")
    if(k == 2) {
      josephusK2(n)
    } else if(n > LARGE_N && k*Math.log(n)/Math.log(2) <= n) {
//      g(n,k) + 1 /// should really use this one, but needs work (TODO), see function g() in tests (ExperimentalSpec).
      fTR(n,k)
    } else {
      fTR(n,k)
    }
  }

  private def leftRotate1Bit(arr: Array[Char]) = {
    val a = arr.toList
    (a.head :: a.tail.reverse).reverse.toArray
  }

  /**
    * Finds the winner given the number of players with k=2.
    * Implementation: it rotates the binary representation of the input number by one digit and returns that.
    * @param n number of players.
    * @return the winner player.
    */
  private[josephus] def josephusK2(n: Int) = {
    val a = n.toBinaryString.toCharArray
    Integer.parseInt(new String(leftRotate1Bit(a)), 2)
  }

  /**
    * After any one out event, the player now in position i, was before in position (i + k - 1) % n + 1.
    * So, in a game with n players, skipping k, if we call the winner f(n,k), then the position f(n,k) in respect the
    * previus out step is:
    * f(n,k) = (f(n-1,k) + k -1) % n + 1
    * which gives the recursive solution (see wikipedia).
    * The problem with this, however, is that can overflow for large n values, as it is not tail recursive.
    * Best to use for small tests or validation.
    * @param n
    * @param k
    * @return
    */
  private[josephus] def f(n: Int, k: Int): Int = {
    if(n==1) 1
    else {
      ((f(n-1, k) + k - 1) % n) + 1
    }
  }


  /**
    * As function f above, but tail recursive.
    * @param n
    * @param k
    * @return
    */
  private[josephus] def fTR(n: Int, k: Int): Int = {
    @tailrec
    def facc(acc: Int, i: Int, k: Int): Int = {
      if (i == 0) acc
      else {
        facc((acc + k - 1) % (n - i + 1) + 1, i - 1, k)
      }
    }
    facc(1, n, k)
  }
}
