package init

import core.Cell

trait Init {
  /**
   * Initializes the Simplex table for a transportation problem
   * by an implemented criteria
   * @param resources Resources (values for the left-hand nodes) 
   * @param demand Demand (values for the right-hand nodes)
   * @param costTable Cost matrix of the assignment of each unit of each resource
   *                  to each demand
   * @return Simplex table with initial Basic Variables
   */
  def run(
           resources: Array[Double],
           demand: Array[Double],
           costTable: Array[Array[Cell]]
         ): Array[Array[Cell]]
}
