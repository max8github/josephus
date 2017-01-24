package org.calderoni.josephus

/**
  * Created by max on 1/22/17. Class represents a node for doubly linked lists.
  * It is used to model a player in a circle of players in this counting out game.
  * Only used for tests and validation.
  *
  * There must be a doubly-linked list already implemented in Scala, but this is easy enough for its scope.
  */
case class BiNode[T](var value: T, var left: BiNode[T] = null, var right: BiNode[T] = null) {
  override def toString: String = value.toString
}

object BiNode {

  /**
    * Creates a circular doubly-linked list.
    * If the list is only one node, the resulting node circles with itself (points to itself).
    *
    * @return the first node originally given in the input list, now linked circularly to all others.
    */
  def generateCircle(dim: Int): BiNode[Int] = {
    require(dim>0, "size of circle must be greater than zero.")
    generateCircle(genList(dim))
  }

  /**
    * Generates n nodes with increasing Int values starting from 1 (could have used List.tabulate, but that starts
    * from zero).
    *
    * @param dim number of nodes.
    * @return list of nodes with increasing integer index starting from index 1. List has size dim.
    */
  private def genList(dim: Int) = {
    var list: List[BiNode[Int]] = Nil
    for (i <- dim to 1 by -1) {
      list = BiNode(i) :: list
    }
    list
  }

  /**
    * Given a list of n children nodes, generates a circular doubly-linked list.
    * If the list is only one node, the resulting node circles with itself (points to itself).
    *
    * @param list non-empty list of children nodes.
    * @return the first node originally given in the input list, now linked circularly to all others.
    */
  private def generateCircle(list: List[BiNode[Int]]): BiNode[Int] = {
    if(list.isEmpty) throw new IllegalArgumentException("Input list cannot be empty.")
    val last = list.tail.foldLeft(list.head) { (c, k) =>
      c.right = k
      k.left = c
      k
    }
    last.right = list.head
    list.head.left = last
    list.head
  }
}