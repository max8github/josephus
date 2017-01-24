package org.calderoni.josephus

import org.calderoni.josephus.BiNode.generateCircle

/**
  * Created by max on 1/23/17.
  */
object JosephusUtil {

  private[josephus] def traverseCircle(bTNode: BiNode[Int]) = {
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
  private[josephus] def josephusList(n: Int, k: Int) = {
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
