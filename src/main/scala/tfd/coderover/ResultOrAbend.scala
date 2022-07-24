package tfd.coderover

import scala.scalajs.js.annotation._


object SuccessResult {
  def apply[A](rawValue:A) = new ResultOrAbend(rawValue)
}

object AbendResult {
  def apply[T](rawAbend:Abend) = new ResultOrAbend[T](rawAbend)
}

@JSExportAll
case class ResultOrAbend[+A](val value:Option[A], val abend:Option[Abend]) {
  def this(rawValue:A) = this(Some(rawValue), None)
  def this(rawAbend:Abend) = this(None, Some(rawAbend))

  def success = !value.isEmpty

  def flatMap[B](f: A => ResultOrAbend[B]): ResultOrAbend[B] =
    if (success) f(value.get) else this.asInstanceOf[ResultOrAbend[B]]

  def map[B](f: A => B): ResultOrAbend[B] =
     if (success) new ResultOrAbend(f(value.get)) else this.asInstanceOf[ResultOrAbend[B]]
}

object SuccessResultUnit extends ResultOrAbend[Any](Some(()), None)