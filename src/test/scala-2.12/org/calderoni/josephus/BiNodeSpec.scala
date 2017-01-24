package org.calderoni.josephus

import org.calderoni.josephus.BiNode.generateCircle
import org.calderoni.josephus.JosephusUtil.traverseCircle
import org.scalatest.{FlatSpec, Ignore, Matchers}

/**
  * Created by max on 1/22/17.
  */
class BiNodeSpec extends FlatSpec with Matchers {

  "A start node" should "be fully traversed in a circle" in {
    val start = generateCircle(9)
    val list = traverseCircle(start)
    val result = list.mkString(",")
    result shouldBe ("1,2,3,4,5,6,7,8,9")
    println(s"Circle: ${result}")
  }
}

