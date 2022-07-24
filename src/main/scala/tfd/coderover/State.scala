package tfd.coderover

import collection.mutable.Stack

import scala.scalajs.js.annotation._

@JSExportTopLevel("State")
@JSExportAll
case class State(
  var gridX:Int,
  var gridY:Int,
  var directionIndex:Int
) {
  import Constants._

  def moveForward() = State(gridX + deltaX, gridY + deltaY, directionIndex)
    
  def turnRight() = State(gridX, gridY,(directionIndex + DIRECTIONS.length + 1) % DIRECTIONS.length)
  
  def turnLeft() = State(gridX, gridY,(directionIndex + DIRECTIONS.length - 1) % DIRECTIONS.length)
      
  def deltaX = DIRECTIONS(directionIndex)._1
  
  def deltaY = DIRECTIONS(directionIndex)._2
}
