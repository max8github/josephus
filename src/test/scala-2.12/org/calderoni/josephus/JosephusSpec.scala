package org.calderoni.josephus

import org.calderoni.josephus.BiNode._
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
      val g = Josephus.g(i, k)
      println(s"$i     $winner, $f, $g")
    }
  }

  "Various circles with k=n" should "work" in {
    for (i <- 1 to 100) {
      val winner = josephusList(i, i)
      val f = Josephus.fTR(i, i)
      val g = Josephus.g(i, i)
      println(s"$i  ->  $winner")
    }
  }

  "A large number of players" should "not cause out of memory" in {
    val n = 1073741824 / 2
    val k = 540
    //    val w = josephusList(n, k)
    val w = Josephus.fTR(n, k)
    //    val w = Josephus.josephusK2(n)
    //    val w = Josephus.g(n, k)
    println(s"($n,$k)  ->  $w")
  }

//  "Circles with g" should "work" in {
//    val n = 2048
//    val k = 4
//    val chooseG = n - (k * Math.log(n) / Math.log(2))
//    println(s"delta: $chooseG")
//    (chooseG > 0) shouldBe true
//    val winner = Josephus.g(n, k) + 1
//    val expected = Josephus.fTR(n, k)
//    println(s"winner $expected is: $winner")
//    winner shouldBe expected
//  }

  "A start node" should "be fully traversed in a circle" in {
    val start = generateCircle(9)
    val list = traverseCircle(start)
    val result = list.mkString(",")
    result shouldBe ("1,2,3,4,5,6,7,8,9")
    println(s"Circle: ${result}")
  }

  private def traverseCircle(bTNode: BiNode[Int]) = {
    var list: List[Int] = bTNode.value :: Nil
    var node = bTNode.right
    while (node != bTNode) {
      list = node.value :: list
      node = node.right
    }
    list.reverse
  }

  /**
    * Runs Josephus' circle with n players and skip k.
    * A bit of a brute force, no-brainer algorithm, but still O(n) (but also O(n) in memory!): used for basic tests.
    * Players are modeled as nodes in doubly-linked list circle. Game runs clockwise.
    *
    * @param n The number of players in the circle.
    * @param k the skip factor.
    * @return the winner node.
    */
  private def josephusList(n: Int, k: Int) = {
    //create circle first
    val startNode = generateCircle(n)
    //current node
    var node = startNode
    //previous node
    var prev = startNode.left
    var gameOn = true
    var j = 1

    while (gameOn) {
      if (j % k == 0) {
        //remove player
        if (node == node.right) {
          //end of the game
          node.left = node
          gameOn = false
        } else {
          //remove kth player, re-wire and increment
          prev.right = node.right
          node.right.left = prev
          node = node.right
        }
      } else {
        //increment step
        prev = prev.right
        node = node.right
      }
      j += 1
    }
    node.value
  }
}

