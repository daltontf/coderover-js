package tfd.coderover

import org.junit.Assert._
import org.junit.Test

import scala.scalajs.js

class StubDelegate extends js.Object with Delegate {
   override def appendStep(step:Object):Unit = ()
}

class EvaluatorTest {
  private[this] val languageParser = new LanguageParser()
  private[this] val evaluator = new Evaluator()
  private[this] val delegate = new StubDelegate()
  private[this] val defaultEnvironment = new Environment(10, 10)
  
  import languageParser._

  private def evaluateInt(intExpression:IntExpression, args:Array[Int] = Array.empty[Int], controller:Controller):ResultOrAbend[Int] =
      evaluator.evaluateInt(intExpression, args, controller)

  private def evaluateInt(intExpression:IntExpression, args:Array[Int], state:State):ResultOrAbend[Int] =
      evaluateInt(intExpression, args, new Controller(state, delegate, defaultEnvironment))

  private def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int] = Array.empty[Int], controller:Controller):ResultOrAbend[Boolean] =
      evaluator.evaluateBoolean(booleanExpression, args, controller)

  private def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int], state:State):ResultOrAbend[Boolean] =
      evaluateBoolean(booleanExpression, args, new Controller(state, delegate, defaultEnvironment))

  private def evaluate(instructions:String, controller:Controller):ResultOrAbend[Any] =
      evaluator.evaluate(parse(instructions).get, controller)

  private def executeConstantTest(stringInput: String, expectedConstant: Constant, expectedInt: Int) {
    val ast = parseAll(constant, stringInput).get
    assertEquals(expectedConstant, ast)
    assertEquals(ResultOrAbend(Some(expectedInt), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeMathematicalTest(stringInput: String, expectedMathematical: Mathematical, expectedInt: Int) {
    val ast = parseAll(mathematical, stringInput).get
    assertEquals(expectedMathematical, ast)
    assertEquals(ResultOrAbend(Some(expectedInt), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeComparisonTest(stringInput: String, expectedComparison: Comparison, expectedBoolean: Boolean) {
    val ast = parseAll(comparison, stringInput).get
    assertEquals(expectedComparison, ast)
    assertEquals(ResultOrAbend(Some(expectedBoolean), None), evaluateBoolean(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeBooleanLogicTest(stringInput: String, expectedBooleanLogic: BooleanExpression, expectedBoolean: Boolean) {
    val ast = parseAll(booleanExpression, stringInput).get
    assertEquals(expectedBooleanLogic, ast)
    assertEquals(ResultOrAbend(Some(expectedBoolean), None), evaluateBoolean(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def executeIntExpressionTest(stringInput: String, expectedAst: Expression, expectedIntResult: Int) {
    val ast = parseAll(intExpression, stringInput).get
    assertEquals(expectedAst, ast)
    assertEquals(ResultOrAbend(Some(expectedIntResult), None), evaluateInt(ast, Array.empty[Int], State(0, 0, 0)))
  }

  private def controllerFromState(state: State) =
    new Controller(state, delegate, defaultEnvironment)
  

  @Test
  def testEmpty() {
    assertEquals(SuccessResultUnit, evaluate("", controllerFromState(State(0,0,0))))
  }

  @Test
  def testConstant() {
    executeConstantTest("42", Constant(42), 42)
    executeConstantTest("-273", Constant(-273), -273)
  }

  @Test
  def testMathematical() {
    executeMathematicalTest("2 + 2", Add(List(Constant(2), Constant(2))), 4)
    executeMathematicalTest("2 + -2", Add(List(Constant(2), Constant(-2))), 0)
    executeMathematicalTest("-2 + 2", Add(List(Constant(-2), Constant(2))), 0)
    executeMathematicalTest("-2 + -2", Add(List(Constant(-2), Constant(-2))), -4)
    executeMathematicalTest("1 + -2 + -3", Add(List(Constant(1), Constant(-2), Constant(-3))), -4)

    executeMathematicalTest("2 - 2", Subtract(List(Constant(2), Constant(2))), 0)
    executeMathematicalTest("2 - -2", Subtract(List(Constant(2), Constant(-2))), 4)
    executeMathematicalTest("-2 - 2", Subtract(List(Constant(-2), Constant(2))), -4)
    executeMathematicalTest("-2 - -2", Subtract(List(Constant(-2), Constant(-2))), 0)
    executeMathematicalTest("-2 - -2 - 3", Subtract(List(Constant(-2), Constant(-2), Constant(3))), -3)

    executeMathematicalTest("-2 * -2 * 3", Multiply(List(Constant(-2), Constant(-2), Constant(3))), 12)

    executeMathematicalTest("24 / 2 / 3", Divide(List(Constant(24), Constant(2), Constant(3))), 4)

    executeMathematicalTest("24 % 5 % 3", Modulus(List(Constant(24), Constant(5), Constant(3))), 1)

    executeMathematicalTest("(2 - 3) + 4", Add(List(Subtract(List(Constant(2), Constant(3))), Constant(4))), 3)
    executeMathematicalTest("2 - (3 + 4)", Subtract(List(Constant(2), Add(List(Constant(3), Constant(4))))), -5)

    executeMathematicalTest("(2 * 3) - 4", Subtract(List(Multiply(List(Constant(2), Constant(3))), Constant(4))), 2)
    executeMathematicalTest("2 - (10 / 3)", Subtract(List(Constant(2), Divide(List(Constant(10), Constant(3))))), -1)
    executeMathematicalTest("4 + (10 % 3)", Add(List(Constant(4), Modulus(List(Constant(10), Constant(3))))), 5)
  }

  @Test
  def testDivByZero() {
    List("PUSH (2/0)",
         "PRINT (2/0)"
      ).map { code:String =>
        assertEquals(ResultOrAbend(None, Some(DivideByZero)), evaluate(code,  controllerFromState(State(2, 2, 0))))
      }
  }

  @Test
  def testModByZero() {
    assertEquals(ResultOrAbend(None, Some(DivideByZero)), evaluate("PUSH (2%0)",  controllerFromState(State(2, 2, 0))))
  }

  @Test
  def testExpression() {
    executeIntExpressionTest("-(2 + 2)", Negate(Add(List(Constant(2), Constant(2)))), -4)
  }

  @Test
  def testComparison() {
    executeComparisonTest("2 = 2", Equal(Constant(2), Constant(2)), true)
    executeComparisonTest("2 = -2", Equal(Constant(2), Constant(-2)), false)

    executeComparisonTest("2 < 2", LessThan(Constant(2), Constant(2)), false)
    executeComparisonTest("-2 < 2", LessThan(Constant(-2), Constant(2)), true)

    executeComparisonTest("2 > 2", GreaterThan(Constant(2), Constant(2)), false)
    executeComparisonTest("2 > -2", GreaterThan(Constant(2), Constant(-2)), true)

    executeComparisonTest("2 <= 2", LessThanOrEqual(Constant(2), Constant(2)), true)
    executeComparisonTest("2 <= -2", LessThanOrEqual(Constant(2), Constant(-2)), false)

    executeComparisonTest("2 >= 2", GreaterThanOrEqual(Constant(2), Constant(2)), true)
    executeComparisonTest("-2 >= 2", GreaterThanOrEqual(Constant(-2), Constant(2)), false)

    executeComparisonTest("2 <> 2", NotEqual(Constant(2), Constant(2)), false)
    executeComparisonTest("2 <> -2", NotEqual(Constant(2), Constant(-2)), true)
  }

  @Test
  def testBooleanLogic() {
    executeBooleanLogicTest("((1 + 3) = (0 - -4))", Equal(Add(List(Constant(1), Constant(3))), Subtract(List(Constant(0), Constant(-4)))), true)
    executeBooleanLogicTest("((1 + 3) <> (0 - -4))", NotEqual(Add(List(Constant(1), Constant(3))), Subtract(List(Constant(0), Constant(-4)))), false)
    executeBooleanLogicTest("((2 > 3) OR (4 > 3))", Or(List(GreaterThan(Constant(2), Constant(3)), GreaterThan(Constant(4), Constant(3)))), true)
    executeBooleanLogicTest("((2 > 3) OR (4 > 5))", Or(List(GreaterThan(Constant(2), Constant(3)), GreaterThan(Constant(4), Constant(5)))), false)
    executeBooleanLogicTest("((4 > 3) AND (5 > 3))", And(List(GreaterThan(Constant(4), Constant(3)), GreaterThan(Constant(5), Constant(3)))), true)
    executeBooleanLogicTest("((2 > 3) AND (5 > 3))", And(List(GreaterThan(Constant(2), Constant(3)), GreaterThan(Constant(5), Constant(3)))), false)
    executeBooleanLogicTest("((2 > 3) OR (4 > 3) OR (2 < 1))", Or(List(GreaterThan(Constant(2), Constant(3)),
      GreaterThan(Constant(4), Constant(3)),
      LessThan(Constant(2), Constant(1)))), true)
    executeBooleanLogicTest("((2 > 3) OR (4 > 5) OR (2 < 1))", Or(List(GreaterThan(Constant(2), Constant(3)),
      GreaterThan(Constant(4), Constant(5)),
      LessThan(Constant(2), Constant(1)))), false)
    executeBooleanLogicTest("((4 > 3) AND (5 > 3) AND (1 < 2))", And(List(GreaterThan(Constant(4), Constant(3)),
      GreaterThan(Constant(5), Constant(3)),
      LessThan(Constant(1), Constant(2)))), true)
  }

  @Test
  def testSimple() {
    val controller = controllerFromState(State(2, 2, 0))
    evaluate("FORWARD", controller)
    assertEquals(State(2, 1, 0), controller.state)
    evaluate("RIGHT", controller)
    assertEquals(State(2, 1, 1), controller.state)
    evaluate("RIGHT", controller)
    assertEquals(State(2, 1, 2), controller.state)
    evaluate("RIGHT", controller)
    assertEquals(State(2, 1, 3), controller.state)
    evaluate("RIGHT", controller)
    assertEquals(State(2, 1, 0), controller.state)
    evaluate("LEFT", controller)
    assertEquals(State(2, 1, 3), controller.state)
    evaluate("LEFT", controller)
    assertEquals(State(2, 1, 2), controller.state)
    evaluate("LEFT", controller)
    assertEquals(State(2, 1, 1), controller.state)
    evaluate("LEFT", controller)
    assertEquals(State(2, 1, 0), controller.state)
    evaluate("RIGHT", controller)
    assertEquals(State(2, 1, 1), controller.state)
  }

  @Test
  def testIfThenElse() {
    val controller = controllerFromState(State(2, 2, 0))
    evaluate("IF (Y = 2) { FORWARD } ELSE { RIGHT }", controller)
    assertEquals(State(2, 1, 0), controller.state)
    evaluate("IF (Y = 2) { FORWARD } ELSE { RIGHT }", controller)
    assertEquals(State(2, 1, 1), controller.state)
    evaluate("IF (X < 3) { FORWARD } ELSE { LEFT }", controller)
    assertEquals(State(3, 1, 1), controller.state)
    evaluate("IF (X < 3) { FORWARD } ELSE { LEFT }", controller)
    assertEquals(State(3, 1, 0), controller.state)
  }

  @Test
  def testIfElseIf() {
    val controller = controllerFromState(State(2, 2, 0))
    evaluate("IF (Y <> 2) { FORWARD } ELSE IF (Y = 2) { RIGHT }", controller)
    assertEquals(State(2, 2, 1), controller.state)
    evaluate("IF (Y = 2) { FORWARD } ELSE IF (Y <> 2) { RIGHT }", controller)
    assertEquals(State(3, 2, 1), controller.state)
    evaluate("IF (Y <> 2) { FORWARD } ELSE IF (Y = 2) { RIGHT }", controller)
    assertEquals(State(3, 2, 2), controller.state)
    evaluate("IF (Y = 2) { FORWARD } ELSE IF (Y <> 2) { RIGHT }", controller)
    assertEquals(State(3, 3, 2), controller.state)
    evaluate("IF (Y = 2) { FORWARD } ELSE IF (X = 2) { RIGHT }", controller)
    assertEquals(State(3, 3, 2), controller.state)
  }

  @Test
  def testStack() {
    val controller = controllerFromState(State(2, 2, 0))
    evaluate("PUSH (1+2)", controller)
    assertEquals(SuccessResult(3), controller.top)
    evaluate("PUSH (1+2) PUSH (TOP + 1)", controller)
    assertEquals(SuccessResult(4), controller.top)
    evaluate("PUSH (1+2) PUSH (TOP + 1) POP", controller)
    assertEquals(SuccessResult(3), controller.top)
    evaluate("PUSH (1+2) PUSH (TOP + 1) POP REPLACE (TOP * 2)", controller)
    assertEquals(SuccessResult(6), controller.top)
  }

  @Test
  def testWhileStack() {
    val controller = controllerFromState(State(2, 2, 0))
    evaluate("""|PUSH 10
    				    |WHILE (TOP > 3) {
    			      |  REPLACE (TOP - 1)
                |}""".stripMargin, controller)
    assertEquals(SuccessResult(3), controller.top)
  }

  @Test
  def testGridXY() {
    val controller = controllerFromState(State(2, 3, 0))
    evaluate("PUSH X PUSH Y", controller)
    assertEquals(SuccessResult(3), controller.top)
    evaluate("PUSH X PUSH Y POP", controller)
    assertEquals(SuccessResult(2), controller.top)
  }

  @Test
  def testMaxXY() {
    val controller = new Controller(State(2, 2, 1), delegate, new Environment(11,9))
    evaluate("PUSH SIZEX", controller)
    assertEquals(SuccessResult(11), controller.top)
    evaluate("PUSH SIZEY", controller)
    assertEquals(SuccessResult(9), controller.top)   
  }

  @Test
  def testWhileGrid() {
    val controller = new Controller(State(2, 2, 1), delegate, new Environment(11,11))
    evaluate("""|WHILE (X < 10) {
		  			    | FORWARD 
		  	  			|}""".stripMargin, controller)
    assertEquals(State(10, 2, 1), controller.state)
    evaluate("""|RIGHT
			  		    |WHILE (Y < 10) {
		  			    | FORWARD 
		  	  			|}""".stripMargin, controller)
    assertEquals(State(10, 10, 2), controller.state)
  }

  @Test
  def testDeltaXY() {
    val controller = controllerFromState(new State(2, 3, 0))
    evaluate("PUSH DX", controller)
    assertEquals(SuccessResult(0), controller.top)
    evaluate("PUSH DY", controller)
    assertEquals(SuccessResult(-1), controller.top)
    evaluate("""|
    			      |RIGHT
    			      |PUSH DX""".stripMargin, controller)
    assertEquals(SuccessResult(1), controller.top)
    evaluate("PUSH DY", controller)
    assertEquals(SuccessResult(0), controller.top)
  }

  @Test
  def testAbs() {
    executeIntExpressionTest("ABS(-1)", Abs(Constant(-1)), 1)
    executeIntExpressionTest("ABS(1)", Abs(Constant(1)), 1)
    executeIntExpressionTest("ABS(3-5)", Abs(Subtract(List(Constant(3), Constant(5)))), 2)
    executeIntExpressionTest("-ABS(3-5)", Negate(Abs(Subtract(List(Constant(3), Constant(5))))), -2)
  }

  @Test
  def testMax() {
    executeIntExpressionTest("MAX(1, 2)", Max(Constant(1), Constant(2)), 2)
    executeIntExpressionTest("MAX(1, -2)", Max(Constant(1), Constant(-2)), 1)
    executeIntExpressionTest("MAX(-1, -2)", Max(Constant(-1), Constant(-2)), -1)
    executeIntExpressionTest("-MAX(-1, -2)", Negate(Max(Constant(-1), Constant(-2))), 1)
  }

  @Test
  def testMin() {
    executeIntExpressionTest("MIN(1, 2)", Min(Constant(1), Constant(2)), 1)
    executeIntExpressionTest("MIN(1, -2)", Min(Constant(1), Constant(-2)), -2)
    executeIntExpressionTest("MIN(-1, -2)", Min(Constant(-1), Constant(-2)), -2)
    executeIntExpressionTest("-MIN(-1, -2)", Negate(Min(Constant(-1), Constant(-2))), 2)
  }

  @Test
  def testBoundedEnvironment() {
    val controller = controllerFromState(State(2, 2, 0))
    evaluate("FORWARD", controller)
    assertEquals(State(2, 1, 0), controller.state)
    evaluate("FORWARD", controller)
    assertEquals(State(2, 0, 0), controller.state)
    evaluate("FORWARD", controller)
    assertEquals(State(2, 0, 0), controller.state)
    evaluate("LEFT REPEAT 2 { FORWARD }", controller)
    assertEquals(State(0, 0, 3), controller.state)
    evaluate("REPEAT 2 { FORWARD }", controller)
    assertEquals(State(0, 0, 3), controller.state)
    evaluate("LEFT REPEAT 9 { FORWARD }", controller)
    assertEquals(State(0, 9, 2), controller.state)
    evaluate("FORWARD", controller)
    assertEquals(State(0, 9, 2), controller.state)
    evaluate("LEFT REPEAT 9 { FORWARD }", controller)
    assertEquals(State(9, 9, 1), controller.state)
    evaluate("FORWARD", controller)
    assertEquals(State(9, 9, 1), controller.state)
    evaluate("LEFT REPEAT 99 { FORWARD }", controller)
    assertEquals(State(9, 0, 0), controller.state)
  }

  @Test
  def testPaint() {
    val environment = new Environment(10,10) {
      import scala.collection.mutable.ListBuffer

      val paintedTuples = new ListBuffer[(Int, Int)]

      override def paint(x:Int, y:Int) {
        paintedTuples += ((x, y))
      }
    }
    val state = new State(2,2,0)
    val controller = new Controller(state, delegate, environment)
    evaluate("PAINT", controller)
    assertEquals((2, 2), environment.paintedTuples(0))
    evaluate("FORWARD PAINT", controller)
    assertEquals((2, 1), environment.paintedTuples(1))
    evaluate("RIGHT FORWARD PAINT", controller)
    assertEquals((3, 1), environment.paintedTuples(2))
  }

  @Test
  def testPainted() {
    val environment = new Environment(10,10) {
      override def isPainted(x: Int, y: Int) = (x, y) == (3, 4)
    }
    val controller = new Controller(State(2, 2, 0), delegate, environment)
    evaluate("""IF (PAINTED(1,2)) { FORWARD }""", controller)
    assertEquals(State(2, 2, 0), controller.state)
    evaluate("""IF (PAINTED(3,4)) { FORWARD }""", controller)
    assertEquals(State(2, 1, 0), controller.state)
  }

  @Test
  def testNot() {
    executeBooleanLogicTest("NOT(4 > 3)", Not(GreaterThan(Constant(4), Constant(3))), false)
    executeBooleanLogicTest("NOT((1 + 3) <> (0 - -4))", Not(NotEqual(Add(List(Constant(1), Constant(3))), Subtract(List(Constant(0), Constant(-4))))), true)
  }

  @Test
  def testIllegalOperationOnEmptyStack() {
    List("POP",
         "PUSH TOP",
         "REPLACE 2"
      ).map { code:String =>
     assertEquals(code, ResultOrAbend(None, Some(IllegalOperationOnEmptyStack)),
          evaluate(code, controllerFromState(State(2, 2, 0))))
    }
  }

  @Test
  def testAdjacent() {
    val controller = new Controller(State(2, 2, 1), delegate,
     new Environment(10,10) {
        override def adjacent(entity: String, x:Int, y:Int) =
        "ROCK" == entity && ((Math.abs(3 - x) + Math.abs(3 - y)) == 1)
      })
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD }""", controller)
    assertEquals(State(2, 2, 1), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(2, 2, 1), controller.state)
    evaluate("""IF (NOT(ADJACENT(ROCK))) { FORWARD }""", controller)
    assertEquals(State(3, 2, 1), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(3, 2, 1), controller.state)
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""", controller)
    assertEquals(State(4, 2, 2), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(4, 2, 2), controller.state)
    evaluate("""IF (NOT(ADJACENT(ROCK))) { FORWARD  }""", controller)
    assertEquals(State(4, 3, 2), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(4, 3, 2), controller.state)
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""", controller)
    assertEquals(State(4, 4, 3), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(4, 4, 3), controller.state)
    evaluate("""IF (NOT(ADJACENT(ROCK))) { FORWARD  }""", controller)
    assertEquals(State(3, 4, 3), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(3, 4, 3), controller.state)
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD RIGHT }""", controller)
    assertEquals(State(2, 4, 0), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(2, 4, 0), controller.state)
    evaluate("""IF (NOT(ADJACENT(ROCK))) { FORWARD }""", controller)
    assertEquals(State(2, 3, 0), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(2, 3, 0), controller.state)
    evaluate("""IF (ADJACENT(ROCK)) { FORWARD }""", controller)
    assertEquals(State(2, 2, 0), controller.state)
    evaluate("""IF (ADJACENT(FLAG)) { FORWARD }""", controller)
    assertEquals(State(2, 2, 0), controller.state)
  }

  @Test
  def testDistances() {
    val state = new State(2, 3, 1)
    val environment = new Environment(10,10) {
      private val entityMap = List(
        "ROCK" -> (5, 5),
        "ROCK" -> (2, 9),
        "FLAG" -> (1, 1),
        "FLAG" -> (8, 2),
        "FLAG" -> (4, 0)
      )

      private def findEntity(entity: String, index:Int) =
        entityMap.filter(_._1 == entity).lift(index)

      override def distanceX(entity: String, index:Int, x:Int, y:Int) =
        for (entity <- findEntity(entity, index)) yield (entity._2._1 - x)

      override def distanceY(entity: String, index:Int, x:Int, y:Int) =
        for (entity <- findEntity(entity, index)) yield (entity._2._2 - y)
    }
    val controller = new Controller(state, delegate, environment)
    evaluate("PUSH DISTANCEX(ROCK)", controller)
    assertEquals(SuccessResult(3), controller.top)
    evaluate("PUSH DISTANCEX(ROCK(1))", controller)
    assertEquals(SuccessResult(3), controller.top)
    evaluate("PUSH DISTANCEX(ROCK(2))", controller)
    assertEquals(SuccessResult(0), controller.top)
    evaluate("PUSH DISTANCEY(ROCK)", controller)
    assertEquals(SuccessResult(2), controller.top)
    evaluate("PUSH DISTANCEY(ROCK(1))", controller)
    assertEquals(SuccessResult(2), controller.top)
    evaluate("PUSH DISTANCEY(ROCK(2))", controller)
    assertEquals(SuccessResult(6), controller.top)
    evaluate("PUSH DISTANCEX(FLAG)", controller)
    assertEquals(SuccessResult(-1), controller.top)
    evaluate("PUSH DISTANCEY(FLAG)", controller)
    assertEquals(SuccessResult(-2), controller.top)
    assertEquals(AbendResult(InvalidEntity("FOO", 1)), evaluate("PUSH DISTANCEX(FOO)", controller))
    assertEquals(AbendResult(InvalidEntity("FOO", 0)), evaluate("PUSH DISTANCEX(FOO(0))", controller))
    assertEquals(AbendResult(InvalidEntity("ROCK",0)), evaluate("PUSH DISTANCEX(ROCK(0))", controller))
    assertEquals(AbendResult(InvalidEntity("ROCK",3)), evaluate("PUSH DISTANCEX(ROCK(3))", controller))
    assertEquals(AbendResult(InvalidEntity("FLAG",4)), evaluate("PUSH DISTANCEX(FLAG(4))", controller))
  }

  @Test
  def testProcCall {
    val controller = controllerFromState(State(2, 2, 0))
    assertEquals(SuccessResultUnit, evaluate(
      """|PROC RIGHTFORWARD { RIGHT FORWARD }
         |RIGHTFORWARD""".stripMargin, controller))
    assertEquals(State(3, 2, 1), controller.state)
    assertEquals(SuccessResultUnit, evaluate("""
         |PROC LEFTFORWARD { LEFT FORWARD }
         |LEFTFORWARD""".stripMargin, controller))
    assertEquals(State(3, 1, 0),  controller.state)
    assertEquals(SuccessResultUnit, evaluate("""
         |PROC EMPTY { }
         |EMPTY""".stripMargin, controller))
    assertEquals(State(3, 1, 0),  controller.state)
    assertEquals(AbendResult(UndefinedProcedure("FOO")), evaluate("FOO", controller))
    assertEquals(State(3, 1, 0),  controller.state)
  }

  @Test
  def testProcCallWithParams {
    val controller = controllerFromState(State(2, 2, 0))
    assertEquals(SuccessResultUnit, evaluate("""
                |PROC RFORWARD { RIGHT REPEAT $1 { FORWARD } }
     					 	|RFORWARD(1)""".stripMargin, controller))
    assertEquals(State(3, 2, 1), controller.state)
    assertEquals(SuccessResultUnit, evaluate("""
                |PROC LFORWARD { LEFT REPEAT $1 { FORWARD } }
                |LFORWARD(2)""".stripMargin, controller))
    assertEquals(State(3, 0, 0), controller.state)
    assertEquals(SuccessResultUnit, evaluate("""
                |PROC LFORWARD { LEFT REPEAT $1 { FORWARD } }
                |LFORWARD(3)""".stripMargin, controller))
    assertEquals(State(0, 0, 3), controller.state)
    assertEquals(SuccessResultUnit, evaluate("""
                |PROC LLFORWARD { LEFT REPEAT $1 {FORWARD} LEFT REPEAT $2 {FORWARD} }
                |LLFORWARD(4, 5)""".stripMargin, controller))
    assertEquals(State(5, 4, 1), controller.state)
    assertEquals(SuccessResultUnit, evaluate("""
                 |PROC EMPTY { }
                 |EMPTY""".stripMargin, controller))
    assertEquals(State(5, 4, 1), controller.state)
    assertEquals(AbendResult(UndefinedProcedure("FOO")), evaluate("FOO", controller))
    assertEquals(State(5, 4, 1), controller.state)

    assertEquals(AbendResult(DivideByZero), evaluate("""
        |PROC GO { REPEAT $1 { FORWARD } }
        |GO(1/0)""".stripMargin, controller))
  }

  @Test
  def testFuncInvoke {
      val controller = controllerFromState(State(2, 2, 0))
      assertEquals(SuccessResultUnit, evaluate("""
        |FUNC PLUSXY ( X + Y )
        |RIGHT
        |REPEAT PLUSXY {FORWARD}""".stripMargin, controller))
      assertEquals(State(6, 2, 1), controller.state)
      assertEquals(AbendResult(DivideByZero), evaluate("""
        |FUNC PLUS_1 ( $1 + 1 )
        |PUSH(PLUS_1(1/0))""".stripMargin, controller))
  }

  @Test
  def testUnboundParams {
    assertEquals(AbendResult(UnboundParameter(1)), evaluate("PUSH($1)", controllerFromState(State(2, 2, 0))))
  }

  @Test
  def testPrint() {
    val state = new State(2, 3, 0)
    val controller = new Controller(state, delegate, defaultEnvironment) {
      var lastPrint: String = null

      override def print(value: String) {lastPrint = value}
    }
    evaluate("""PRINT "X = " . X . " Y = " . Y . " " . ((2+2) = 4) . " foo" """, controller)
    assertEquals("X = 2 Y = 3 true foo", controller.lastPrint)
  }            

  @Test
  def testStoreMem() {
    val controller = new Controller(new State(2, 2, 0), delegate, defaultEnvironment, new Constraints(10, 10, 10))
    assertEquals(SuccessResultUnit, evaluate("""STORE (3,42) PUSH MEM(3)""", controller))
    assertEquals(SuccessResult(42), controller.top)
    assertEquals(42, controller.executionState.memory(3))
    assertEquals(AbendResult(InvalidMEMAddress(10)), evaluate("""STORE (10,42)""", controller))
    assertEquals(AbendResult(InvalidMEMAddress(11)), evaluate("""PUSH MEM(11)""", controller))
  }

  @Test
  def testMaxStack() {
    val state = new State(2, 2, 0)
    val controller = new Controller(state, delegate, defaultEnvironment) //, new Constraints(10, 1, 10))
    evaluate("""PUSH 1""", controller)
    //assertEquals(None, state.abend)
    evaluate("""PUSH 1""", controller)
    //assertEquals(Some(StackOverflow), state.abend)
  }

  @Test
  def testMaxCallStack() {
    val state = new State(2, 2, 0)
    val controller = new Controller(state, delegate, defaultEnvironment) // new Constraints(10, 10, 1))
    evaluate("""
    |PROC FOO { PUSH 2 POP }
    |PROC BAR { PUSH 1 FOO POP }
    |BAR""".stripMargin, controller)
    //assertEquals(Some(CallStackOverflow), state.abend)
  }

  @Test
  def testObstructions() {
    val state = new State(2, 2, 2)
    val environment = new Environment(10, 10) {
      override def isObstructed(x: Int, y: Int) = (x,y) == (2,3)
    }
    val controller = new Controller(state, delegate, environment)
    assertEquals(SuccessResultUnit, evaluate("FORWARD", controller))
    assertEquals(State(2,2,2), state)
  }

  @Test
  def testObstructed() {
    val environment = new Environment(3, 3) {
      override def isObstructed(x: Int, y: Int) = (x,y) == (1,1)
    }
    val controller = new Controller(new State(0, 0, 0), delegate, environment)

    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,0,1), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(1,0,1), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,0,1), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,0,2), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,1,2), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,2,2), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(2,2,3), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(1,2,3), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,2,3), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,2,0), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,1,0), controller.state)
    // Face obstructed square first
    assertEquals(SuccessResultUnit, evaluate("RIGHT IF NOT(OBSTRUCTED(X+DX,Y+DY)) { FORWARD } ELSE { RIGHT }", controller))
    assertEquals(State(0,1,2), controller.state)
    // Test outside grid boundaries
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(-1,0)) { RIGHT }", controller))
    assertEquals(State(0,1,3), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(0,-1)) { RIGHT }", controller))
    assertEquals(State(0,1,0), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(0,0)) { RIGHT }", controller))
    assertEquals(State(0,1,0), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(2,3)) { RIGHT }", controller))
    assertEquals(State(0,1,1), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(3,2)) { RIGHT }", controller))
    assertEquals(State(0,1,2), controller.state)
    assertEquals(SuccessResultUnit, evaluate("IF (OBSTRUCTED(2,2)) { RIGHT }", controller))
    assertEquals(State(0,1,2), controller.state)
  }

  // @Test
  // def testPostMoveForward() {
  //   object Kablooey extends Abend("Kablooey")
    
  //   val environment = new Environment(3, 3);
  //   val controller = new Controller(new State(0, 0, 0), delegate, environment) {
  //     override def postMoveForward():Option[Abend] = {

  //       state match {
  //         case State(1,1,_) => Some(Kablooey)
  //         case _ => None
  //       }
  //     }
  //   }
  //   assertEquals(SuccessResultUnit, evaluate("RIGHT FORWARD RIGHT", controller))
  //   assertEquals(State(1,0,2), controller.state)
  //   assertEquals(AbendResult(Kablooey), evaluate("FORWARD", controller))
  // }

  @Test
  def testTernary() {
    val controller = controllerFromState(State(2, 2, 0))
    evaluate("PUSH ((1 < 2) ? 1 : 2)", controller)
    assertEquals(SuccessResult(1), controller.top)
    evaluate("PUSH ((1 > 2) ? 1 : 2)", controller)
    assertEquals(SuccessResult(2), controller.top)
  }

  @Test
  def testCount() {
    val controller = new Controller(
      State(2, 2, 0),  delegate,
      new Environment(10,10) {
        override def count(entity: String) = if (entity == "FOO") Some(42) else None
      }
    )
    evaluate("PUSH COUNT(FOO)", controller)
    assertEquals(SuccessResult(42), controller.top)
    assertEquals(AbendResult(UnknownEntity("BAR")), evaluate("PUSH COUNT(BAR)", controller))
  }

  @Test
  def testRecursiveFunc() {
    val controller = new Controller(new State(2, 3, 0), delegate, defaultEnvironment) {
      var lastPrint: String = null

      override def print(value: String) {lastPrint = value}
    }
    evaluate("""
      |FUNC FACTORIAL ( ($1 = 1) ? 1 : $1 * FACTORIAL($1 - 1))
      |PRINT "FACTORIAL(6) = " . FACTORIAL(6)""".stripMargin, controller)
    assertEquals("FACTORIAL(6) = 720", controller.lastPrint)
  }

  @Test
  def testParamCount() {
    val controller = controllerFromState(new State(2, 3, 0))
    evaluate("PUSH $COUNT", controller)
    assertEquals(SuccessResult(0), controller.top)
    evaluate("PROC FOO { PUSH $COUNT } FOO(1)", controller)
    assertEquals(SuccessResult(1), controller.top)
    evaluate("PROC FOO { PUSH $COUNT } FOO(1,2,3)", controller)
    assertEquals(SuccessResult(3), controller.top)
  }

  @Test
  def testPred() {
    val controller = controllerFromState(new State(2, 2, 0))
    evaluate("""
      |PRED Y_EQUALS (Y = $1)
      |IF Y_EQUALS(2) { FORWARD }""".stripMargin, controller)
    assertEquals(State(2,1,0), controller.state)
    evaluate("""
       |PRED Y_EQUALS (Y = $1)
       |IF Y_EQUALS(2) { FORWARD }""".stripMargin, controller)
    assertEquals(State(2,1,0), controller.state)
    evaluate("""
       |PRED Y_EQUALS (Y = $1)
       |IF Y_EQUALS(1) { FORWARD RIGHT }""".stripMargin, controller)
    assertEquals(State(2,0,1), controller.state)

    assertEquals(AbendResult(DivideByZero), evaluate("""
      |PRED Y_EQUALS (Y = $1)
      |IF Y_EQUALS(1/0) { FORWARD RIGHT }""".stripMargin, controller))
  }

  @Test
  def testUnboundInProc() {
    assertEquals(AbendResult(UndefinedProcedure("BAR")),
      evaluate("""
        |PROC FOO { BAR(1) }
        |FOO""".stripMargin, controllerFromState(new State(0,0,0)))        
    )
  }

  @Test
  def testUnboundPred() {
    val controller = controllerFromState(new State(2, 2, 0))
    assertEquals(AbendResult(UndefinedPredicate("Y_EQUAL")), evaluate("""
      |PRED Y_EQUALS (Y = $1)
      |IF Y_EQUAL(2) { FORWARD }""".stripMargin, controller))
  }

  // @Test
  // def testFailureInRepeat() {
  //   val controller = new Controller(new State(2, 2, 0), delegate, defaultEnvironment) {
  //     override def postMoveForward():Option[Abend] = state match {
  //       case State(4,2,_) => Some(UnknownEntity("FOO"))
  //       case _ => None
  //     }
  //   }
  //   assertEquals(AbendResult(UnknownEntity("FOO")), evaluate("""
  //     |RIGHT REPEAT 5 { FORWARD }""".stripMargin, controller))
  //   assertEquals(State(4,2,1), controller.state)
  // }

  // @Test
  // def testFailureInIf() {
  //   val controller = new Controller(new State(2, 2, 0), delegate, defaultEnvironment) {
  //     override def postMoveForward():Option[Abend] = state match {
  //       case State(x,_,_) if x > 2 => Some(UnknownEntity("FOO"))
  //       case _ => None
  //     }
  //   }
  //   assertEquals(AbendResult(DivideByZero), evaluate("""
  //     |IF ((1 / 0) = 1) {
  //     |}""".stripMargin, controller))
    
  //   assertEquals(AbendResult(UnknownEntity("FOO")), evaluate("""
  //     |IF (1 = 1) {
  //     |  RIGHT REPEAT 5 { FORWARD }
  //     |}""".stripMargin, controller))
  //   assertEquals(State(3,2,1), controller.state)
  // }

  // @Test
  // def testFailureInWhile() {
  //   val controller = new Controller(new State(2, 2, 0), delegate, defaultEnvironment) {
  //     override def postMoveForward():Option[Abend] = state match {
  //       case State(x,_,_) if x > 2 => Some(UnknownEntity("FOO"))
  //       case _ => None
  //     }
  //   }
  //   assertEquals(AbendResult(DivideByZero), evaluate("""
  //     |WHILE ((1 / 0) = 1) {
  //     |}""".stripMargin, controller))
    
  //   assertEquals(AbendResult(UnknownEntity("FOO")), evaluate("""
  //     |PUSH 2
  //     |RIGHT
  //     |WHILE (TOP > 0) {
  //     |  REPLACE (TOP - 1)
  //     |  FORWARD
  //     |}""".stripMargin, controller))
  //   assertEquals(State(3,2,1), controller.state)
  // }
    
  @Test
  def testShortCircuitAnd() {
    assertEquals(
      SuccessResult(false),
      evaluateBoolean(And(List(Equal(Constant(1), Constant(2)), Equal(Constant(1), Mem(Constant(-1))))), Array.empty[Int], (State(2,2,0)))
    )
  }

  @Test
  def testShortCircuitOr() {
    assertEquals(
      SuccessResult(true),
      evaluateBoolean(Or(List(Equal(Constant(1), Constant(1)), Equal(Constant(1), Mem(Constant(-1))))), Array.empty[Int], (State(2,2,0)))
    )
  }
}
