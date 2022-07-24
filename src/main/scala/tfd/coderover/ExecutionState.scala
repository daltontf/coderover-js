package tfd.coderover

import collection.mutable.Stack

class ExecutionState(constraints:Constraints = DefaultConstraints) {
  private[coderover] var callStackSize = 0

  private[coderover] def decrementCallStack() {
    callStackSize = callStackSize - 1
  }

  private[coderover] def incrementCallStack() {
    callStackSize = callStackSize + 1
  }

  private[coderover] def resetCallStack() {
    callStackSize = 0
  }

  private[coderover] val procMap = new scala.collection.mutable.HashMap[String, List[Instruction]]()

  private[coderover] val funcMap = new scala.collection.mutable.HashMap[String, IntExpression]()

  private[coderover] val predMap = new scala.collection.mutable.HashMap[String, BooleanExpression]()

  private[coderover] val stack = new Stack[Int]()

  private[coderover] val memory = new Array[Int](constraints.memorySize)

  private[coderover] var stopped = false

  memory.indices.map{ (i:Int) => memory(i) =  0 }
}