package tfd.coderover

import collection.mutable.Stack
import annotation.tailrec

import scala.scalajs.js.annotation._

@JSExportTopLevel("Evaluator")
class Evaluator() {
  
  @JSExport
  final def evaluate(instructions:List[Instruction], controller:Controller):ResultOrAbend[Any] = {
    controller.resetState
    evaluate(instructions, Array.empty[Int], controller)
  }  

  private[this] final def evaluate(instructions:List[Instruction], args:Array[Int], controller:Controller):ResultOrAbend[Any] =
    if (instructions != Nil) {
      instructions.tail.foldLeft(evaluateInstruction(instructions.head, args, controller)) {
         (previousResult, instruction) =>
            for (xp <- previousResult; yp <- evaluateInstruction(instruction, args, controller))
              yield (yp)
      }
    } else {
      SuccessResultUnit
    }

  
  private[this] final def processDistance(distance:Option[Int], index:Int, entity:String):ResultOrAbend[Int] =
    if (distance == None) { 
    	 AbendResult(InvalidEntity(entity, index))
    } else {
       SuccessResult(distance.get)
    }

  private[this] def evaluateDivideByZero(expression:IntExpression, args:Array[Int], controller:Controller):ResultOrAbend[Int] = {
    val result = evaluateInt(expression, args, controller)
    if (!result.value.isEmpty && result.value.get == 0) {
      AbendResult(DivideByZero)
    } else {
      result
    }
  }

  private[coderover] def foldLeftInt(expressionList:List[IntExpression], args:Array[Int], controller:Controller, f:(Int, Int) => Int) =
    expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
        (previousResult, intExpression) =>
          for (xp <- previousResult; yp <- evaluateInt(intExpression, args, controller))
            yield f(xp,yp)
    }

  private[coderover] def foldLeftIntDivide(expressionList:List[IntExpression], args:Array[Int], controller:Controller, f:(Int, Int) => Int) =
    expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
        (previousResult, intExpression) =>
          for (xp <- previousResult; yp <- evaluateDivideByZero(intExpression, args, controller))
            yield f(xp,yp)
    }

  private[coderover] final def evaluateInt(expression:IntExpression, args:Array[Int], controller:Controller):ResultOrAbend[Int] = {
    expression match {
      case Constant(x)               => SuccessResult(x)
      case Add(expressionList) 	 	   => foldLeftInt(expressionList, args, controller, _ + _ )
      case Subtract(expressionList)  => foldLeftInt(expressionList, args, controller, _ - _ )
      case Multiply(expressionList)  => foldLeftInt(expressionList, args, controller, _ * _ )
      case Divide(expressionList)    => foldLeftIntDivide(expressionList, args, controller, _ / _ )
      case Modulus(expressionList)   => foldLeftIntDivide(expressionList, args, controller, _ % _ )
      case Top() 				             => controller.top
      case GridX() 				           => SuccessResult(controller.gridX)
      case GridY() 				           => SuccessResult(controller.gridY)
      case DeltaX() 			           => SuccessResult(controller.deltaX)
      case DeltaY() 			           => SuccessResult(controller.deltaY)
      case Depth()				           => SuccessResult(controller.depth)
      case Abs(expr) 			           => for (x <- evaluateInt(expr, args, controller)) yield (Math.abs(x))
      case Max(expr1, expr2) 	       => for (x <- evaluateInt(expr1, args, controller);
                                             y <- evaluateInt(expr2, args, controller)) yield (Math.max(x,y))
      case Min(expr1, expr2) 	       => for (x <- evaluateInt(expr1, args, controller);
                                             y <- evaluateInt(expr2, args, controller)) yield (Math.min(x,y))
      case Negate(expr)			         => for (x <- evaluateInt(expr, args, controller)) yield (-x)
      case DistanceX(entity, expr)   => for (index <- evaluateInt(expr, args, controller);
                                             distance <- processDistance(controller.distanceX(entity, index - 1), index, entity)) yield (distance)
      case DistanceY(entity, expr)   => for (index <- evaluateInt(expr, args, controller);
                                             distance <- processDistance(controller.distanceY(entity, index - 1), index, entity)) yield (distance)
      case Count(entity)  	         => controller.count(entity) match {
                                          case Some(x) => SuccessResult(x)
                                          case None => AbendResult(UnknownEntity(entity))
                                        }
      case Mem(address)              => for (x <- evaluateInt(address, args, controller);
                                             y <- controller.mem(x)) yield (y)
      case EvalParam(expr)           => for (position <- evaluateInt(expr, args, controller);
                                             result <- if (position > 0 && position <= args.length) {
                                                SuccessResult(args(position-1))
                                              } else {
                                                AbendResult(UnboundParameter(position))
                                              }) yield(result)
      case ParamCount()              => SuccessResult(args.length)
      case InvokeFunc(name, invArgs) => if (controller.executionState.funcMap.contains(name)) {
                                        controller.incrementCallStack()
                                        val evalArgs = invArgs.map{ evaluateInt(_, args, controller) }
                                        val failedArgEval = evalArgs.find{ !_.success }
                                        val result:ResultOrAbend[Int] = if (failedArgEval.isEmpty) {
                                          evaluateInt(
                                            controller.executionState.funcMap(name),
                                            evalArgs.map {_.value.get }.toArray,
                                            controller)
                                          } else {
                                            AbendResult(failedArgEval.get.abend.get)
                                          }
                                        controller.executionState.decrementCallStack()
                                        result
                                        } else {
                                          AbendResult(UndefinedFunction(name))
                                        }
      case Ternary(booleanExpression, thenExpression, elseExpression) =>
            for (x <- evaluateBoolean(booleanExpression, args, controller);
                 res <-evaluateInt(if (x) thenExpression else elseExpression, args, controller))
              yield { res }
    }
  }
  
   private[coderover] final def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int], controller:Controller):ResultOrAbend[Boolean] = {
	  booleanExpression match {
        case Obstructed(xExpression, yExpression) => for (x <- evaluateInt(xExpression, args, controller);
                                                          y <- evaluateInt(yExpression, args, controller))
                                                          yield (
                                                              (x < 0) ||
                                                              (y < 0) ||
                                                              (x >= controller.sizeX) ||
                                                              (y >= controller.sizeY) ||
                                                              controller.isObstructed(x,y) )
        case Painted(xExpression, yExpression)    => for (x <- evaluateInt(xExpression, args, controller);
                                                          y <- evaluateInt(yExpression, args, controller))
                                                          yield (controller.isPainted(x, y))
        case Not(booleanExpression)		            => for (x <- evaluateBoolean(booleanExpression, args, controller))
                                                          yield ( !x )
	      case And(booleanExpressionList)		        => booleanExpressionList.tail.foldLeft(evaluateBoolean(booleanExpressionList.head, args, controller)) {
                                                        (previousResult, booleanExpression) =>
                                                          for (
                                                            xp <- previousResult;
                                                            yp <- if (xp) evaluateBoolean(booleanExpression, args, controller) else SuccessResult(false))
                                                            yield (xp && yp)
                                                     }
        case Or(booleanExpressionList) 		        => booleanExpressionList.tail.foldLeft(evaluateBoolean(booleanExpressionList.head, args, controller)) {
                                                        (previousResult, booleanExpression) =>
                                                          for (
                                                            xp <- previousResult;
                                                            yp <- if (!xp) evaluateBoolean(booleanExpression, args, controller) else SuccessResult(true))
                                                            yield (xp || yp)
                                                     }
        case Equal(left, right) 			            => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x == y)
        case LessThan(left, right) 			          => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x < y)
        case GreaterThan(left, right) 	          => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x > y)
        case LessThanOrEqual(left, right) 	      => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x <= y)
        case GreaterThanOrEqual(left, right)      =>  for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x >= y)
        case NotEqual(left, right) 			          =>  for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x != y)
        case Adjacent(entity)				              =>  SuccessResult(controller.isAdjacent(entity))

        case InvokePred(name, invArgs)    => if (controller.executionState.predMap.contains(name)) {
                                        controller.incrementCallStack()
                                        val evalArgs = invArgs.map{ evaluateInt(_, args, controller) }
                                        val failedArgEval = evalArgs.find{ !_.success }
                                        val result:ResultOrAbend[Boolean] = if (failedArgEval.isEmpty) {
                                          evaluateBoolean(
                                            controller.executionState.predMap(name),
                                            evalArgs.map {_.value.get }.toArray,
                                            controller)
                                          } else {
                                            AbendResult(failedArgEval.get.abend.get)
                                          }
                                        controller.executionState.decrementCallStack()
                                        result
                                        } else {
                                          AbendResult(UndefinedPredicate(name))
                                        }
	  }
  }
   
  private[coderover] final def evaluateString(expression:Expression, args:Array[Int], controller:Controller):ResultOrAbend[String] = {
	   expression match {
        case StringConstant(value)					      => SuccessResult(value)
        case intExpression:IntExpression          => for (x <- evaluateInt(intExpression, args, controller))
                                                        yield(x.toString)
        case booleanExpression:BooleanExpression  => for (x <- evaluateBoolean(booleanExpression, args, controller))
                                                        yield(x.toString)
	   }  
  }

  @JSExport
  final def evaluateInstruction(instruction:Instruction, args:Array[Int], controller:Controller):ResultOrAbend[Any] = {
      if (!controller.executionState.stopped) {
        instruction match {
            case Proc(name, instructions) => {
                                                controller.executionState.procMap += name -> instructions
                                                SuccessResultUnit
                                            }
            case InvokeProc(name, callArgs)    => if (controller.executionState.procMap.contains(name)) {
                                                controller.incrementCallStack()
                                                val evalArgs = callArgs.map{ evaluateInt(_, args, controller) }
                                                val failedArgEval = evalArgs.find{ !_.success }
                                                val result:ResultOrAbend[Any] = if (failedArgEval.isEmpty) {
                                                  evaluate(
                                                      controller.executionState.procMap(name),
                                                      evalArgs.map {_.value.get }.toArray,
                                                      controller)
                                                } else {
                                                  AbendResult(failedArgEval.get.abend.get)
                                                }
                                                controller.executionState.decrementCallStack()
                                                result
            				                        } else {
            					   	                    AbendResult(UndefinedProcedure(name))
            				                        }
        	  case Forward()               => {
                                             val result = controller.moveForward()
                                             if (result.isEmpty) {
                                                SuccessResultUnit
                                              } else {
                                                AbendResult(result.get)
                                              }
                                            }
        	  case TurnRight()             => {
                                              controller.turnRight()
                                              SuccessResultUnit
                                            }
        	  case TurnLeft()	             => {
                                              controller.turnLeft()
                                              SuccessResultUnit
                                            }
        	  case Push(expression)        => for (x <- evaluateInt(expression, args, controller);
                                                 result <- controller.push(x))
                                              yield (result)
            case Pop() 			             => controller.pop
            case Replace(expression)     => for (x <- evaluateInt(expression, args, controller);
            	                                   _ <- controller.pop();
                                                 result <- controller.push(x))
                                                       yield (result)
        	case If(booleanExpression, thenstatements, elsestatements) => {
            val booleanResult = evaluateBoolean(booleanExpression, args, controller)
            if (booleanResult.success) {
              if (booleanResult.value.get) {
                evaluate(thenstatements, args, controller)
              } else if (!elsestatements.isEmpty) {
        			  evaluate(elsestatements, args, controller)
              } else {
                SuccessResultUnit
              }
            } else {
              booleanResult
            }
          }
        	case While(booleanExpression, blockstatements) => {
            var result:ResultOrAbend[Any] = SuccessResultUnit
            var booleanResult:ResultOrAbend[Boolean] = SuccessResult(true)
        		while (result.success && booleanResult.success && booleanResult.value.get) {
            	booleanResult = evaluateBoolean(booleanExpression, args, controller)
              if (booleanResult.success && booleanResult.value.get) {
                result = evaluate(blockstatements, args, controller)
              }
        		}
            if (booleanResult.success) result else booleanResult
          }
          case Paint() => {
              controller.paint()
              SuccessResultUnit
          }
          case Print(expressionList) => for (evaluationResult <- expressionList.tail.foldLeft(evaluateString(expressionList.head, args, controller)) {
                                                        (previousResult, stringExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateString(stringExpression, args, controller))
                                                            yield (xp + yp)
                                           }) yield { controller.print(evaluationResult) }
          case Store(address, value) => for (x <- evaluateInt(address, args, controller);
                                             y <- evaluateInt(value, args, controller);
                                             result <- controller.store(x, y))
                                        yield (result)
          case Func(name, expression) => {
                                                controller.executionState.funcMap += name -> expression
                                                SuccessResultUnit
                                         }
          case Pred(name, expression) => {
                                                controller.executionState.predMap += name -> expression
                                                SuccessResultUnit
                                         }
          case Repeat(timesExpression, instructions) => {
                val timesResult = evaluateInt(timesExpression, args, controller)
                if (timesResult.success) {
                  var times = timesResult.value.get
                  var repeatResult:ResultOrAbend[Any] = SuccessResultUnit
                  while (times > 0 && repeatResult.success) {
                    repeatResult = evaluate(instructions, args, controller)
                    times -= 1
                  }
                  repeatResult
                } else {
                  AbendResult(InvalidRepeat(timesResult.abend.get.message))
                }
              }
        }
      } else {
          ResultOrAbend(None, None)
      }
  }
}
