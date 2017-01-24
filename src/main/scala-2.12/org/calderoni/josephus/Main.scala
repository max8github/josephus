package org.calderoni.josephus

/**
  * Created by max on 1/22/17.
  */
object Main extends App {
  val msg = "Arguments to specify must be two integers: number of players and skip value. Example: run 31 2"
  try {
    require(args.length == 2, msg)
    val out = Josephus.run(args(0).toInt, args(1).toInt)
    println(s"$out")
  } catch {
    case ex: NumberFormatException => println(s"INPUT ERROR: $msg")
    case ex: IllegalArgumentException => println(s"INPUT ERROR: ${ex.getMessage}")
  }
}
