package core

import init.{NorthwestCorner, Voguel}

import java.util.Scanner

object Runner {
  def main(args: Array[String]): Unit = {
    val read = new Scanner(System.in)

    println("Introduce each resource value (space-separated):")
    val resources = read
      .nextLine()
      .split(" ")
      .map(_.toDouble)

    println("Introduce each demand value (space-separated):")
    val demand = read
      .nextLine()
      .split(" ")
      .map(_.toDouble)

    println("Introduce costs (space-separated rows). Introduce any non numerical value to stop:")
    var costInput = List.empty[Array[Cell]]
    try {
      while (true) {
        costInput = costInput.appended(read
          .nextLine()
          .split(" ")
          .map(x => NoInit(
            if x == "M" then Double.PositiveInfinity
            else x.toDouble
          ))
        )
      }
    }
    catch {
      case _ : IllegalArgumentException =>
    }
    val costs = costInput.toArray
    val init = NorthwestCorner.run(resources, demand, costs)

    println("Initialization result")
    init.foreach(x => println(x.mkString(",")))
  }
}
