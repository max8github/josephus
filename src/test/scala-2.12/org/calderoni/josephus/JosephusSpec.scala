package org.calderoni.josephus

import org.calderoni.josephus.BiNode._
import org.calderoni.josephus.JosephusUtil._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by max on 1/22/17.
  */
class JosephusSpec extends FlatSpec with Matchers {

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

  "A start node" should "be fully traversed in a circle" in {
    val start = generateCircle(9)
    val list = traverseCircle(start)
    val result = list.mkString(",")
    result shouldBe ("1,2,3,4,5,6,7,8,9")
    println(s"Circle: ${result}")
  }
}

