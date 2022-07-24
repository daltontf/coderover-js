package tfd.coderover

import scala.scalajs.js.annotation._

@JSExportTopLevel("Environment")
@JSExportAll
class Environment(
        val sizeX:Int,
        val sizeY:Int
  ) {

  val obstructed = scala.collection.mutable.HashSet[(Int,Int)]()
  val prePainted = scala.collection.mutable.HashSet[(Int,Int)]()
  var targetLocation:Option[(Int,Int)] = None
  val visibleEntities = scala.collection.mutable.HashMap[String, Set[(Int, Int)]]()
  val hiddenEntities = scala.collection.mutable.HashMap[String, Set[(Int, Int)]]()


  def addObstruction(x: Int, y:Int) {
    obstructed += ((x, y))
  }  

  def isObstructed(x:Int, y:Int) = obstructed.contains((x,y))
  
  def paint(x:Int, y:Int) {
    painted(x)(y) = true
  }

  def obstruct(x:Int, y:Int) { }

  def count(entity:String) = visibleEntities.get(entity).map(_.size)

  def adjacent(entity:String, x:Int, y:Int) = {
    val entityLocations = hiddenEntities.getOrElse(entity, Set.empty) ++ visibleEntities.getOrElse(entity, Set.empty)
    entityLocations.contains((x - 1, y)) ||
    entityLocations.contains((x + 1, y)) ||
    entityLocations.contains((x, y - 1)) ||
    entityLocations.contains((x, y + 1))
  }

  def isPainted(x:Int, y:Int):Boolean =
    if (x < 0 || y < 0 || x >= sizeX || y >= sizeY) {
      false
    } else {
      painted(x)(y)
    }

  private def findEntity(entity: String, index:Int):Option[(Int, Int)] =
    for (set <- visibleEntities.get(entity);
         pair <- set.toIndexedSeq.lift(index)) yield (pair)

  def distanceX(entity:String, index:Int, x:Int, y:Int):Option[Int] =
    findEntity(entity, index) map (x - _._1)


  def distanceY(entity:String, index:Int, x:Int, y:Int):Option[Int] =
     findEntity(entity, index) map (y - _._2)  

  protected var painted:Array[Array[Boolean]] = _

  def reset() {
    painted = Array.ofDim[Boolean](sizeX, sizeY)
    prePainted.foreach { tuple => paint(tuple._1, tuple._2) }
  }

  reset()
}
