package org.calderoni.josephus

import org.scalatest.{FlatSpec, Ignore, Matchers}

/**
  * Created by max on 1/22/17.
  */
//@Ignore//on the works
class ExperimentalSpec extends FlatSpec with Matchers {

  "Function g with O(k*log(n)) solution" should "work" in {
    val n = 2048
    val k = 4
    val chooseG = n - (k * Math.log(n) / Math.log(2))
    println(s"delta: $chooseG")
    (chooseG > 0) shouldBe true
    val winner = g(n, k) + 1
    val expected = Josephus.fTR(n, k)
    val expected2 = JosephusUtil.josephusList(n, k)
    println(s"winner $expected $expected2 is: $winner")
    winner shouldBe expected
  }

  /**
    * Implementation of an O(k*log(n)) implementation of Josephus, as described in wikipedia at:
    * https://en.wikipedia.org/wiki/Josephus_problem
    * Does not work. TODO: Keep around for later fixing: wikipedia may have to be fixed as well...
    * TODO: once it works, may need to make it tail recursive for very large numbers.
    * @param n
    * @param k
    * @return
    */
  def g(n: Int, k: Int): Int = {
    if(n==1) {
      0
    } else if(n>1 && k>n) {
      (g(n-1, k) + k) % n
    } else {//(k<=n)
      val n1 = n - Math.floor(n/k).toInt
      val d = g(n1,k) - n % k
      val dk = d % n1
      val v = k*dk/(k-1)
      Math.floor(v).toInt
    }
  }
}

