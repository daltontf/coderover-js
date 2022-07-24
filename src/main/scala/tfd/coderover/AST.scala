package tfd.coderover

import scala.util.parsing.input.Positional

sealed abstract class Instruction extends Positional {

//  def updatePosition(result:ResultOrAbend[_]):ResultOrAbend[_] = {
//    result.position = pos;
//  }

}
case class Forward() extends Instruction
case class TurnRight() extends Instruction
case class TurnLeft() extends Instruction
case class If(booleanExpression:BooleanExpression, thenStatements:List[Instruction], elseStatements:List[Instruction]) extends Instruction
case class While(booleanExpression:BooleanExpression, blockStatements:List[Instruction]) extends Instruction
case class Push(expression:IntExpression) extends Instruction
case class Paint() extends Instruction
case class Pop() extends Instruction
case class Replace(expression:IntExpression) extends Instruction
case class Proc(name:String, statements:List[Instruction]) extends Instruction
case class Func(name:String, expression:IntExpression) extends Instruction
case class Pred(name:String, expression:BooleanExpression) extends Instruction
case class InvokeProc(name:String, callArgs:List[IntExpression]) extends Instruction
case class Print(expression:List[Expression]) extends Instruction
case class Store(address:IntExpression, value:IntExpression) extends Instruction
case class Repeat(times:IntExpression, instructions:List[Instruction]) extends Instruction

sealed abstract class Expression()
  case class StringConstant(value:String) extends Expression

  sealed abstract class IntExpression() extends Expression
    case class Constant(value:Int) extends IntExpression
    case class Top() extends IntExpression
    case class GridX() extends IntExpression
    case class GridY() extends IntExpression
    case class DeltaX() extends IntExpression
    case class DeltaY() extends IntExpression
    case class Depth() extends IntExpression
    case class DistanceX(entity:String, indexExpression:IntExpression) extends IntExpression
    case class DistanceY(entity:String, indexExpression:IntExpression) extends IntExpression
    case class Count(entity:String) extends IntExpression
    case class Abs(expression:IntExpression) extends IntExpression
    case class Max(expression1:IntExpression, expression2:IntExpression) extends IntExpression
    case class Min(expression1:IntExpression, expression2:IntExpression) extends IntExpression
    case class Negate(expression:IntExpression) extends IntExpression
    case class Mem(expression:IntExpression) extends IntExpression
    case class EvalParam(expression:IntExpression) extends IntExpression
    case class ParamCount() extends IntExpression
    case class InvokeFunc(name:String, args:List[IntExpression]) extends IntExpression
    case class Ternary(booleanExpression:BooleanExpression, thenExpression:IntExpression, elseExpression:IntExpression) extends IntExpression


 sealed abstract class Mathematical(expressions:List[IntExpression]) extends IntExpression
  case class Add(expressions:List[IntExpression]) extends Mathematical(expressions)
  case class Subtract(expressions:List[IntExpression]) extends Mathematical(expressions)
  case class Multiply(expressions:List[IntExpression]) extends Mathematical(expressions)
  case class Divide(expressions:List[IntExpression]) extends Mathematical(expressions)
  case class Modulus(expressions:List[IntExpression]) extends Mathematical(expressions)

sealed abstract class BooleanExpression() extends Expression
 case class Painted(x:IntExpression, y:IntExpression) extends BooleanExpression
 case class Adjacent(entity:String) extends BooleanExpression
 case class Not(booleanExpression:BooleanExpression) extends BooleanExpression
 case class Obstructed(x:IntExpression, y:IntExpression) extends BooleanExpression
 case class InvokePred(name:String, args:List[IntExpression]) extends BooleanExpression
 sealed abstract class Logical() extends BooleanExpression
  case class Or(expressions:List[BooleanExpression])extends Logical
  case class And(expressions:List[BooleanExpression]) extends Logical
 sealed abstract class Comparison(left:IntExpression, right:IntExpression) extends BooleanExpression
  case class LessThan(left:IntExpression, right:IntExpression) extends Comparison(left, right)
  case class GreaterThan(left:IntExpression, right:IntExpression) extends Comparison(left, right)
  case class Equal(left:IntExpression, right:IntExpression) extends Comparison(left, right)
  case class GreaterThanOrEqual(left:IntExpression, right:IntExpression) extends Comparison(left, right)
  case class LessThanOrEqual(left:IntExpression, right:IntExpression) extends Comparison(left, right)
  case class NotEqual(left:IntExpression, right:IntExpression) extends Comparison(left, right)
