package core

sealed trait Cell {
  val cost: Double
  val assignment: Double

  def isM: Boolean = cost.isInfinite
}

case class BV (
                override val cost: Double, 
                override val assignment: Double
              ) extends Cell
              
case class NBV (
                 override val cost: Double,
                 override val assignment: Double = 0
               ) extends Cell
               
case class NoInit (
                    override val cost: Double
                  ) extends Cell {
  override val assignment: Double = 0
}