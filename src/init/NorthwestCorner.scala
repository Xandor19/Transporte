package init

import core.{BV, Cell, NBV, NoInit}

/**
 * Performs the Northwest Corner initialization for the Simplex Method of 
 * transportation problems
 */
object NorthwestCorner extends Init {
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
    var i = 0
    var j = 0

    while (resources(i) > 0 || demand(j) > 0) {
      val increment = Math.min(resources(i), demand(j))

      result(i)(j) = costTable(i)(j) match {
        case NoInit(cost) => BV(cost = cost, assignment = increment)
        case _ => throw new IllegalStateException("Can't reach an initialized variable during process")
      }
      if (resources(i) - increment > 0) {
        resources(i) -= increment
        demand(j) = 0
        j += 1
      }
      else if (demand(j) - increment > 0) {
        demand(j) -= increment
        resources(i) = 0
        i += 1
      }
      else {
        resources(i) = 0
        demand(j) = 0

        if demand.exists(_.!=(0)) || resources.exists(_.!=(0)) then j += 1
      }
    }
    result
  }
}
