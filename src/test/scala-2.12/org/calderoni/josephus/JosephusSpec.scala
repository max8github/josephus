package org.calderoni.josephus

import org.calderoni.josephus.BiNode._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by max on 1/22/17.
  */
class JosephusSpec extends FlatSpec with Matchers {

  "A circle" should "be fully traversed" in {
    val start = generateCircle(9)
    val list = traverseCircle(start)
    val result = list.mkString(",")
    result shouldBe ("1,2,3,4,5,6,7,8,9")
    println(s"Circle: ${result}")
  }

  "Loop with given k varying n" should "tabulate" in {
    val k = 3
    println("\nn      winner")
    println("---------")
    for (i <- 1 to 300) {
      val circle = generateCircle(i)
      val winner = josephusList(circle, k)
      println(s"$i     $winner")
    }
  }

  "Circles with k=n" should "work" in {
    for (i <- 1 to 100) {
      val circle = generateCircle(i)
      val winner = josephusList(circle, i)
      println(s"$i  ->  $winner")
    }
  }

  def traverseCircle(bTNode: BiNode[Int]) = {
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
    * Players are modeled as nodes in doubly-linked list circle. Game runs clockwise.
    * @param startNode The starting player in the circle.
    * @param k the skip factor.
    * @return the winner node.
    */
  def josephusList(startNode: BiNode[Int], k: Int) = {
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
    node
  }
}

