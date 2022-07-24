package tfd.coderover

import scala.scalajs.js.annotation._

@JSExportTopLevel("Constraints")
class Constraints(
        val memorySize:Int,
        val maxStackSize:Int,
        val maxCallStackSize:Int)

object DefaultConstraints extends Constraints(256, 1024, 256)