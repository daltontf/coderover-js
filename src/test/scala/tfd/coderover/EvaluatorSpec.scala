// package tfd.coderover

// import wvlet.airspec._

// class EvaluatorSpec extends AirSpec {
//   private[this] val languageParser = new LanguageParser()
//   private[this] val evaluator = new Evaluator()

//   import languageParser._

//   private def evaluateInt(intExpression:IntExpression, args:Array[Int] = Array.empty[Int], controller:Controller):ResultOrAbend[Int] =
//       evaluator.evaluateInt(intExpression, args, controller)

//   private def evaluateInt(intExpression:IntExpression, args:Array[Int], state:State):ResultOrAbend[Int] =
//       evaluateInt(intExpression, args, new Controller(state, defaultEnvironment))

//   private def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int] = Array.empty[Int], controller:Controller):ResultOrAbend[Boolean] =
//       evaluator.evaluateBoolean(booleanExpression, args, controller)

//   private def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int], state:State):ResultOrAbend[Boolean] =
//       evaluateBoolean(booleanExpression, args, new Controller(state, defaultEnvironment))

//   private def evaluate(instructions:String, controller:Controller):ResultOrAbend[Any] =
//       evaluator.evaluate(parse(instructions).get, controller)

// //  "Evaluating TOP" should {
// //    "fail on empty stack in" {
// //      val controller = new Controller(State(2, 2, 0))
// //
// //
// //    }
// //  }
// }