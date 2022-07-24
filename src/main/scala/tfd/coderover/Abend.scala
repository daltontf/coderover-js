package tfd.coderover

abstract class Abend(val message:String)

object IllegalOperationOnEmptyStack extends Abend("Illegal operation performed on empty stack")

object DivideByZero extends Abend("Divide by zero")

object StackOverflow extends Abend("Stack overflow")

object CallStackOverflow extends Abend("Call Stack overflow")

object GridLocationOutOfBounds extends Abend("Grid location out of bounds")

case class InvalidMEMAddress(val value:Int) extends Abend("Invalid MEM address :" + value)

case class InvalidRepeat(wrappedMessage:String) extends Abend("Invalid Repeat :" + wrappedMessage)

case class UnknownEntity(val entity:String) extends Abend("Unknown entity :" + entity)

case class InvalidEntity(val entity:String, index:Int) extends Abend("Invalid entity :" + entity + "(" + index + ")")

case class UndefinedProcedure(val name:String) extends Abend("Undefined PROCEDURE :" + name)

case class UndefinedFunction(val name:String) extends Abend("Undefined FUNCTION :" + name)

case class UndefinedPredicate(val name:String) extends Abend("Undefined PREDICATE :" + name)

case class UnboundParameter(val position:Int) extends Abend("Unbound parameter :" + position)


