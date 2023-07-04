package init

import core.{BV, Cell, NBV, NoInit}

/**
 * Performs the Voguel initialization for the Simplex Method of
 * transportation problems
 */
object Voguel extends Init {
  /**
   * Initializes the Simplex table for a transportation problem
   * by an implemented criteria
   * @param resources Resources (values for the left-hand nodes)
   * @param demand Demand (values for the right-hand nodes)
   * @param costTable Cost matrix of the assignment of each unit of each resource
   *                  to each demand
   * @return Simplex table with initial Basic Variables
   */
  override def run(
                    resources: Array[Double],
                    demand: Array[Double],
                    costTable: Array[Array[Cell]]
                  ): Array[Array[Cell]] = {
    val result: Array[Array[Cell]] = costTable.map(_.map(x => NBV(x.cost)))
    var rows = resources.length
    var cols = demand.length
    var iter = 0

    while (rows > 1 && cols > 1) {
      var i = 0
      var j = 0

      val (maxRowDiff, maxRowInd) = costTable
        .map(x => diffCalculator(x))
        .maxWithIndex
      val (maxColDiff, maxColInd) = costTable
        .transpose
        .map(x => diffCalculator(x))
        .maxWithIndex

      if (maxRowDiff > maxColDiff) {
        i = maxRowInd
        j = costTable(maxRowInd).indexOfMin
      }
      else {
        i = costTable
          .transpose
          .apply(maxColInd)
          .indexOfMin
        j = maxColInd
      }
      val increment = Math.min(resources(i), demand(j))

      result(i)(j) = costTable(i)(j) match {
        case NoInit(cost) => BV(cost = cost, assignment = increment)
        case _ => throw new IllegalStateException("Can't reach an initialized variable during selection process")
      }
      if (resources(i) - increment > 0) {
        resources(i) -= increment
        removeColumn(costTable, demand, j)
        cols -= 1
      }
      else if (demand(j) - increment > 0) {
        demand(j) -= increment
        removeRow(costTable, resources, i)
        rows -= 1
      }
      else {
        removeRow(costTable, resources, i)
        removeColumn(costTable, demand, j)
        cols -= 1
        rows -= 1
      }
      println(s"Iteration: $iter")
      costTable.foreach(x => println(x.mkString(",")))
      println(s"Demands: ${demand.mkString(",")}")
      println(s"Resources: ${resources.mkString(",")}")
      println()
      iter += 1
    }
    if (rows == 1) {
      val index = costTable.indexWhere(_.exists(x => x.isInstanceOf[NoInit]))
      result(index) = result(index)
        .zip(demand)
        .collect {
          case (BV(cost, assignment), dem) => BV(cost, assignment)
          case (NBV(cost, 0), dem) => BV(cost, dem)
          case _ => throw new IllegalStateException("Can't reach a non init cell during last row fill")
        }
    }
    else {
      val index = costTable
        .transpose
        .indexWhere(_.exists(x => x.isInstanceOf[NoInit]))

      for (i <- resources.indices) {
        result(i)(index) = result(i)(index) match {
          case BV(cost, assignment) => BV(cost, assignment)
          case NBV(cost, 0) => BV(cost, resources(i))
          case _ => throw new IllegalStateException("Can't reach a non init cell during last column fill")
        }
      }
    }
    result
  }

  def removeRow(table: Array[Array[Cell]], resources: Array[Double], row: Int): Unit = {
    table(row) = Array.fill(table(row).length)(NBV(Double.NaN))
    resources(row) = 0
  }

  def removeColumn(table: Array[Array[Cell]], demands: Array[Double], col: Int): Unit = {
    for (ind <- table.indices) {
      table(ind)(col) = NBV(Double.NaN)
    }
    demands(col) = 0
  }

  /**
   * Calculates the difference between the smallest and second smallest
   * costs of a row or column of the Simplex table
   * @param x The row or column in its original state
   * @return The difference bewteen the two smallest elements by cost
   */
  def diffCalculator(x: Array[Cell]): Double = {
    val sorted = x.sortBy(_.cost)

    if sorted(1).isM && sorted(0).isM then Double.NegativeInfinity
    else sorted(1).cost - sorted(0).cost
  }

  /**
   * Adds the functionality of obtaining the max element along with
   * its index to a Double-type Array, which represents the differences
   * between lesser costs in rows and columns
   * @param seq The extended collection
   */
  implicit class GreaterDifferenceIndex(seq: Array[Double]) {
    /**
     * Obtains the maximum element of the collection along with
     * the index in which is located
     * @return Tuple with (MaxElement, Index)
     */
    def maxWithIndex: (Double, Int) = {
      var maxInd = 0
      var max = 0D

      for (i <- seq.indices) {
        if (seq(i) > max) {
          maxInd = i
          max = seq(i)
        }
      }
      (max, maxInd)
    }
  }

  /**
   * Adds the functionality of obtaining the min element's index
   * to a Cell-type Array, which represents the costs of a given
   * row or column of the Simplex table
   * @param seq The extended collection
   */
  implicit class LessCostCellIndex(seq: Array[Cell]) {
    /**
     * Obtains the index of the minimum element of the collection
     * @return Index in which the minimum element is located
     */
    def indexOfMin: Int = {
      var minInd = 0
      var min = Double.PositiveInfinity

      for (i <- seq.indices) {
        if (seq(i).cost < min) {
          minInd = i
          min = seq(i).cost
        }
      }
      minInd
    }
  }
}
