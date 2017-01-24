package org.calderoni.josephus

import org.calderoni.josephus.JosephusUtil._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by max on 1/22/17.
  */
class JosephusSpec extends FlatSpec with Matchers {

  //  an [IllegalArgumentException] should be thrownBy Josephus.run(0,1)
  //  an [IllegalArgumentException] should be thrownBy Josephus.run(1,0)

  "Invalid parameters" should "be caught" in {
    val winner = Josephus.run(1, 1)
    println(s"winner is $winner")
    val thrown = intercept[IllegalArgumentException] {
      Josephus.run(0, 3)
    }
    assert(thrown.getMessage === "requirement failed: number of players should be strictly positive")
    intercept[IllegalArgumentException] {
      Josephus.run(128, 0)
    }
  }

  "n=3, k=2" should "work" in {
    val k = 2
    val n = 3
    val f = Josephus.run(n, k)
    val winner = josephusList(n, k)
    f shouldBe winner
    println(s"$winner")
  }

  "For a given k, varying n, the Josephus game" should "match all given algorithms" in {
    val k = 3
    println("\nn      winner")
    println("---------")
    for (i <- 1 to 300) {
      val winner = josephusList(i, k)
      val f = Josephus.fTR(i, k)
      f shouldBe winner
      println(s"$i     $winner, $f")
    }
  }

  "Various circles with k=n" should "work" in {
    for (i <- 1 to 100) {
      val winner = josephusList(i, i)
      val f = Josephus.fTR(i, i)
      f shouldBe winner
      println(s"$i  ->  $winner")
    }
  }

  "A large number of players" should "not cause out of memory" in {
    val n = 268435456 / 2
    val k = 540
    val w = Josephus.fTR(n, k)
    w shouldBe 80440826
    println(s"($n,$k)  ->  $w")
  }

  "Any n with k=2" should "be fast" in {
    val n = 1073741824 / 2
    val w = Josephus.josephusK2(n)
    w shouldBe 1
    println(s"($n,2)  ->  $w")
  }
}

