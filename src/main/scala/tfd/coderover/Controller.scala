package tfd.coderover

import scala.scalajs.js
import scala.scalajs.js.annotation._


trait Delegate extends js.Any {
  def appendStep(step:Object):Unit 
}

@js.native
trait JSDelegate extends Delegate {
  def appendStep(step:Object):Unit = js.native
}

@JSExportAll
case class PaintEvent(gridX:Int, gridY:Int);

@JSExportAll
case class PrintEvent(text:String);

@JSExportTopLevel("Controller")
class Controller(@JSExport var state:State,
                 val delegate: Delegate,
                 environment:Environment,
                 constraints:Constraints = DefaultConstraints) {
  var executionState:ExecutionState = _

  def resetState() {
    executionState = new ExecutionState(constraints)
  }

  def moveForward():Option[Abend] = {
    if (!executionState.stopped && canMoveForward) {
      executeMoveForward()
    } 
    None
  }

  private def canMoveForward(state:State) = {
    val nextX = state.gridX + state.deltaX
    val nextY = state.gridY + state.deltaY
    (nextX >= 0 && nextX < sizeX && nextY >=0 && nextY < sizeY && !isObstructed(nextX, nextY))
  }

  def executeMoveForward() { state = state.moveForward(); delegate.appendStep(state); }

  def turnRight() { state = state.turnRight(); delegate.appendStep(state); }

  def turnLeft() { state = state.turnLeft(); delegate.appendStep(state); }

  def paint() { environment.paint(gridX, gridY); delegate.appendStep(PaintEvent(gridX, gridY)); }

  def print(value:String) { delegate.appendStep(PrintEvent(value)) }

  def push(value:Int):ResultOrAbend[Any] =  {
    executionState.stack.push(value)
    if (executionState.stack.size > constraints.maxStackSize) {
      delegate.appendStep(PrintEvent("StackOverflow"));
      AbendResult(StackOverflow)      
    } else {
      SuccessResultUnit
    }
  }

  def pop():ResultOrAbend[Any] =
    if (!executionState.stack.isEmpty) {
      executionState.stack.pop()
      SuccessResultUnit
    } else {
      delegate.appendStep(PrintEvent("IllegalOperationOnEmptyStack"));
      AbendResult(IllegalOperationOnEmptyStack)     
    }

  def top:ResultOrAbend[Int] =
    if (!executionState.stack.isEmpty) {
      SuccessResult(executionState.stack.top)
    } else {
      delegate.appendStep(PrintEvent("IllegalOperationOnEmptyStack"));
      AbendResult(IllegalOperationOnEmptyStack)     
    }

  def depth = executionState.stack.size

  def incrementCallStack():ResultOrAbend[Any] = {
    executionState.incrementCallStack()
    if (executionState.callStackSize > constraints.maxCallStackSize) {
      delegate.appendStep(PrintEvent("CallStackOverflow"));
      AbendResult(CallStackOverflow)
    } else {
      SuccessResultUnit
    }
  }

  def mem(address:Int):ResultOrAbend[Int] =
    if (address >= 0 && address < executionState.memory.size) {
      SuccessResult(executionState.memory(address))
    } else {
      delegate.appendStep(PrintEvent("InvalidMEMAddress(" + address + ")"));
      AbendResult(InvalidMEMAddress(address));      
    }

  def store(address:Int, value:Int):ResultOrAbend[Any] = {
    if (address >= 0 && address < executionState.memory.size) {
      SuccessResult(executionState.memory(address) = value)
    } else {
      delegate.appendStep(PrintEvent("InvalidMEMAddress(" + address + ")"));
      AbendResult(InvalidMEMAddress(address));      
    }
  }

  val sizeX = environment.sizeX

  val sizeY = environment.sizeY

  def distanceX(entity:String, index:Int):Option[Int] = environment.distanceX(entity, index, gridX, gridY)

  def distanceY(entity:String, index:Int):Option[Int] = environment.distanceY(entity, index, gridX, gridY)

  def count(entity:String):Option[Int] = environment.count(entity)

  def isObstructed(x:Int, y:Int) = environment.isObstructed(x,y)

  def isPainted(x:Int, y:Int) = environment.isPainted(x, y)

  def isAdjacent(entity:String) = environment.adjacent(entity, gridX, gridY)

  def canMoveForward:Boolean = canMoveForward(state)

  def gridX = state.gridX

  def gridY = state.gridY

  def deltaX = state.deltaX

  def deltaY = state.deltaY

  def stop() { if (executionState != null) executionState.stopped = true }

  def stopped = executionState != null && executionState.stopped
}
