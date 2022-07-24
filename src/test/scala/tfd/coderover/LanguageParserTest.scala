package tfd.coderover

import org.junit.Assert._
import org.junit.Test

class LanguageParserTest {
  private[this] val languageParser = new LanguageParser();  import languageParser._

  private def assertParsingWithParserProduces(parser:Parser[_])(expectedAst:Expression, code:Seq[String]) {
    code.foreach{ x =>
      val parseResult = parseAll(parser, x)
      if (!parseResult.successful) {
        fail("Parsing failed:\n" + parseResult.asInstanceOf[NoSuccess].msg)
      }
      assertEquals(expectedAst, parseResult.get)
    }
  }

  private def assertProgramParsingProduces(expectedAst:List[Any], code:Seq[String]) {
    code.foreach{ x =>
      val parseResult = parse(x)
      if (!parseResult.successful) {
        fail("Parsing failed:\n" + parseResult.asInstanceOf[NoSuccess].msg)
      }
      assertEquals(expectedAst, parseResult.get)
    }
  }

  private def assertProgramParsingProduces(expectedAst:List[Any], code:String) {
    assertProgramParsingProduces(expectedAst, Seq(code)) 
  }

  @Test
  def testEmptyProgram() {
    assertProgramParsingProduces(List(), Seq("", " ","\n", "\n \n"))
  }

  @Test
  def testIntExpression() {
    val assertParsingIntExpression = assertParsingWithParserProduces(intExpression) _
    assertParsingIntExpression(Constant(1), Seq("1", "01", "(1)", "((1))", "(((01)))", "((0000001))"))
    assertParsingIntExpression(Constant(-1), Seq("-1", "-01", "(-1)", "((-01))", "((-00001))"))
    assertParsingIntExpression(Negate(Add(List(Constant(1), Constant(2)))), Seq("-(1+2)", "-(((1)+(2)))"))
  }

  @Test
  def testStringConstant() {
    assertParsingWithParserProduces(stringConstant)(StringConstant("foo"), Seq("\"foo\""))
  }

  @Test
  def testMathematical() {
    val assertParsingMathematical = assertParsingWithParserProduces(mathematical) _
    assertParsingMathematical(Add(List(Constant(1), Constant(-1))), Seq("1 + -1"))
    assertParsingMathematical(Subtract(List(Constant(1), Constant(-1))), Seq("1 - -1"))
    assertParsingMathematical(Subtract(List(Add(List(Constant(-1), Constant(2))), Constant(3))), Seq("(-1 + 2) - 3"))
    assertParsingMathematical(Multiply(List(Constant(4), Constant(5))), Seq("4 * 5"))
    assertParsingMathematical(Divide(List(Constant(10), Constant(3))), Seq("10 / 3"))
    assertParsingMathematical(Modulus(List(Constant(10), Constant(3))), Seq("10 % 3"))
  }

  @Test
  def testComparison() {
    val assertParsingComparison = assertParsingWithParserProduces(comparison) _
    assertParsingComparison(Equal(Constant(1), Constant(2)), Seq("1 = 2"))
    assertParsingComparison(LessThan(Constant(-1), Constant(1)), Seq("-1 < 1"))
    assertParsingComparison(GreaterThan(Constant(3), Constant(-2)), Seq(" 3  >  -2"))
    assertParsingComparison(LessThanOrEqual(Constant(-1), Constant(1)), Seq("-1 <= 1"))
    assertParsingComparison(GreaterThanOrEqual(Constant(3), Constant(-2)), Seq("3>=-2"))
    assertParsingComparison(NotEqual(Constant(1), Constant(2)), Seq("1<>2"))
  }

  @Test
  def testLogical() {
    val assertParsingLogical = assertParsingWithParserProduces(logical) _
    assertParsingLogical(And(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)))), Seq("(2 = 2) AND (4 <> -3)"))
    assertParsingLogical(Or(List(
      Equal(Constant(2), Constant(3)),
      NotEqual(Constant(4), Constant(-3)))), Seq("(2 = 3) OR (4 <> -3)"))
    assertParsingLogical(And(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)),
      LessThan(Constant(4), Constant(3)))), Seq("(2 = 2) AND (4 <> -3) AND (4 < 3)"))
    assertParsingLogical(Or(List(
      Equal(Constant(2), Constant(2)),
      NotEqual(Constant(4), Constant(-3)),
      LessThan(Constant(4), Constant(3)))), Seq("(2 = 2) OR (4 <> -3) OR (4 < 3)"))
  }

  @Test
  def testForward() {
    assertProgramParsingProduces(List(Forward()), "FORWARD")
  }

  @Test
  def testRightLeft() {
    assertProgramParsingProduces(List(TurnRight(), TurnLeft()), 
      """|RIGHT
    	   |LEFT""".stripMargin)
  }

  @Test
  def testSimpleIfComparisons() {
    assertProgramParsingProduces(List(If(Equal(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 = 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(LessThan(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)), 
      """|
         |IF (1 < 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(GreaterThan(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 > 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(GreaterThanOrEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 >= 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(LessThanOrEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      """|
         |IF (1 <= 2) {                                                                                                               
         | LEFT
         | FORWARD
         | }""".stripMargin)
    assertProgramParsingProduces(List(If(NotEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward()), Nil)),
      Seq("""|
         |IF (1 <> 2) {
         | LEFT
         | FORWARD
         | }""".stripMargin,
    // unnecessary params
      """|
         |IF ((1 <> 2)) {
         | LEFT
         | FORWARD
         | }""".stripMargin))
  }

  @Test
  def testIfElse() {
    assertProgramParsingProduces(List(If(NotEqual(Constant(1), Constant(2)), List(TurnLeft(), Forward()), List(TurnRight()))), 
      """|
         |IF (1 <> 2) {
         | LEFT
         | FORWARD
         |} ELSE {
         | RIGHT
         |}""".stripMargin)
  }

  @Test
  def testIfElseIf() {
    assertProgramParsingProduces(List(
      If(NotEqual(Constant(1), Constant(2)),
        List(TurnLeft(), Forward()),
        List(If(Equal(Constant(2), Constant(3)), List(TurnRight()), Nil))
        )),
        """|
           |IF (1 <> 2) {
           | LEFT
           | FORWARD
           |} ELSE IF (2 = 3) {
           | RIGHT
           |}""".stripMargin)
  }

  @Test
  def testLogicalComparisonAndMathematical() {
    assertProgramParsingProduces(List(
      If(
        And(List(
          GreaterThan(Constant(1), Constant(-1)),
          NotEqual(Constant(-3), Add(List(Constant(2), Constant(-2)))))),
        List(TurnLeft(), Forward(), TurnRight()), Nil)),
      """|
         |IF ((1 > -1) AND (-3 <> (2 + -2))) {
         | LEFT
         | FORWARD
         | RIGHT
         | }""".stripMargin)
  }

  @Test
  def testWhile() {
    assertProgramParsingProduces(List(While(Equal(Constant(1), Constant(2)), List())),
      Seq("""|
         |WHILE (1 = 2) {
         |}""".stripMargin,
      """|
         |WHILE ((1 = 2)) {
         |}""".stripMargin))
    assertProgramParsingProduces(List(While(Not(Equal(Constant(1), Constant(2))), List())), 
      Seq("""|
         |WHILE NOT(1 = 2) {
         |}""".stripMargin,
      """|
         |WHILE (NOT(1 = 2)) {
         |}""".stripMargin, 
      """|
         |WHILE NOT(1 = 2) {
         |}""".stripMargin))
  }

  @Test
  def testPush() {
    assertProgramParsingProduces(List(Push(Constant(1))), "PUSH 1")
    assertProgramParsingProduces(List(Push(Add(List(Constant(1), Constant(2))))), "PUSH (1+2)")
    assertProgramParsingProduces(List(Push(Subtract(List(Constant(8), GridX())))), "PUSH (8 - X)")
    assertProgramParsingProduces(List(Push(Negate(Add(List(Constant(1), Constant(2)))))), "PUSH -(1+2)")
    assertProgramParsingProduces(List(Push(Abs(Constant(-1)))), "PUSH ABS(-1)")
  }

  @Test
  def testPop() {
    assertProgramParsingProduces(List(Push(Constant(1)), Pop()), "PUSH 1 POP")
  }

  @Test
  def testReplace() {
    assertProgramParsingProduces(List(Push(Constant(1)), Replace(Multiply(List(Top(), Constant(2))))),
      """|PUSH 1
    	   |REPLACE (TOP * 2)""".stripMargin)
  }

  @Test
  def testTop() {
    assertProgramParsingProduces(List(If(LessThanOrEqual(Top(), Constant(-1)), List(), Nil)),
      """|IF (TOP <= -1) {
		  	 |}""".stripMargin)
  }

  @Test
  def testDepth() {
    assertProgramParsingProduces(List(Push(Add(List(Depth(), Constant(1))))), "PUSH (DEPTH + 1)")
  }

  @Test
  def testGridX() {
    assertProgramParsingProduces(List(While(LessThan(GridX(), Constant(5)), List(Forward()))),
      """|WHILE (X < 5) {
         | FORWARD
		     |}""".stripMargin)
  }

  @Test
  def testGridY() {
    assertProgramParsingProduces(List(While(GreaterThanOrEqual(GridY(), Constant(1)), List(Forward()))),
      """|WHILE (Y >= 1) {
         | FORWARD
		     |}""".stripMargin)
  }

  @Test
  def testDeltaX() {
    assertProgramParsingProduces(List(While(Equal(DeltaX(), Constant(1)), List(TurnRight()))),
      """|WHILE (DX = 1) {
         | RIGHT
		  	 |}""".stripMargin)
  }

  @Test
  def testDeltaY() {
    assertProgramParsingProduces(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      """|WHILE (DY = 0) {
         | LEFT
		  	 |}""".stripMargin)
  }

  @Test
  def testCount() {
    assertProgramParsingProduces(List(Push(Count("FOO"))), "PUSH COUNT(FOO)")
  }

  @Test
  def testAbs() {
    assertProgramParsingProduces(List(Push(Abs(Constant(-1)))), "PUSH ABS(-1)")
    assertProgramParsingProduces(List(Push(Negate(Abs(Constant(-1))))), "PUSH -ABS(-1)")
    assertProgramParsingProduces(List(Push(Abs(Negate(Add(List(Constant(1), Constant(2), Constant(3))))))), "PUSH ABS(-(1 + 2 + 3))")
  }

  @Test
  def testMax() {
    assertProgramParsingProduces(List(Push(Max(Constant(-1), Constant(2)))), "PUSH MAX(-1, 2)")
  }

  @Test
  def testMin() {
    assertProgramParsingProduces(List(Push(Min(Constant(-1), Constant(2)))), "PUSH MIN(-1, 2)")
  }

  @Test
  def testPaint() {
    assertProgramParsingProduces(List(Paint()), "PAINT")
  }

  @Test
  def testPainted() {
    assertProgramParsingProduces(List((While(Painted(Constant(1), Constant(2)), List(Forward())))),
      "WHILE (PAINTED(1,2)) { FORWARD }")
    assertProgramParsingProduces(List((While(Painted(Constant(1), Constant(2)), List(Forward())))),
      "WHILE PAINTED(1,2) { FORWARD }")
    assertProgramParsingProduces(List((While(Painted(Constant(1), Constant(2)), List(Forward())))),
      "WHILE ((PAINTED(1,2))) { FORWARD }")

  }

  @Test
  def testNot() {
    assertProgramParsingProduces(List((While(Not(Painted(Constant(1), Constant(2))), List(Forward())))),
      "WHILE (NOT(PAINTED(1,2))) { FORWARD }")
  }

  @Test
  def testAdjacent() {
    assertProgramParsingProduces(List(If(Adjacent("MINE"), List(TurnRight()), Nil)), "IF (ADJACENT(MINE)) { RIGHT }")
  }

  @Test
  def testDistances() {
    assertProgramParsingProduces(List(Push(DistanceX("FLAG", Constant(1)))), "PUSH DISTANCEX(FLAG)")
    assertProgramParsingProduces(List(Push(DistanceY("ROCK",  Constant(2)))), "PUSH DISTANCEY(ROCK(2))")
    assertProgramParsingProduces(List(Push(Min(DistanceX("FLAG", Constant(3)), DistanceY("FLAG",  Constant(3))))),
      "PUSH MIN(DISTANCEX(FLAG(3)), DISTANCEY(FLAG(3)))")
  }

  @Test
  def testProc() {
    assertProgramParsingProduces(List(Proc("TWO_LEFTS", List(TurnLeft(), TurnLeft()))), "PROC TWO_LEFTS { LEFT LEFT }")
    assertProgramParsingProduces(List(Proc("TWO_LEFTS", List(TurnLeft(), TurnLeft()))), "PROCEDURE TWO_LEFTS { LEFT LEFT }")
    assertProgramParsingProduces(List(Proc("TWO_LEFTS_underscore", List(TurnLeft(), TurnLeft()))), "PROC TWO_LEFTS_underscore { LEFT LEFT }")
  }

  @Test
  def testCall() {
    assertProgramParsingProduces(List(InvokeProc("PRC", Nil)), Seq("PRC", "PRC()"))
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42)))), "PRC(42)")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42), Add(List(Top(), Constant(28)))))), "PRC(42,TOP + 28)")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42), Add(List(Top(), Constant(28)))))), "PRC(42,(TOP + 28))")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42), Negate(Add(List(Top(), Constant(28))))))), "PRC(42,-(TOP + 28))")
    assertProgramParsingProduces(List(InvokeProc("PRC", List(Constant(42), Add(List(Negate(Top()), Constant(28)))))), "PRC(42,-TOP + 28)")
  }

  @Test
  def testPrint() {
    assertProgramParsingProduces(List(Print(List(StringConstant("X = "), GridX()))), """PRINT "X = " . X""")
    assertProgramParsingProduces(List(Print(List(StringConstant("SUM = "), Add(List(Top(), Constant(28)))))), """PRINT "SUM = " . (TOP + 28)""")
    assertProgramParsingProduces(List(Print(List(StringConstant("PAINTED = "), Painted(Constant(1), Constant(2))))), """PRINT "PAINTED = " . PAINTED(1,2)""")
  }

  @Test
  def testStore() {
    assertProgramParsingProduces(List(Store(Constant(1), GridX())), Seq("STORE (1, X)", "STORE((1),(X))"))
  }

  @Test
  def testMem() {
    assertProgramParsingProduces(List(Push(Mem(Constant(1)))), Seq("PUSH(MEM(1))", "PUSH((MEM((1))))"))
  }

  @Test
  def testComments() {
    assertProgramParsingProduces(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      """|WHILE (DY = 0) { // comment
         | LEFT
         |}//comment""".stripMargin)
    assertProgramParsingProduces(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      """|WHILE /* multi-line
         | comment */ (DY = 0) {
         | LEFT  /* Comment */
		     |}""".stripMargin)
    assertProgramParsingProduces(List(While(Equal(DeltaY(), Constant(0)), List(TurnLeft()))),
      """|WHILE (DY = 0) { // Comment
         | /* Comment */ LEFT
		     |}""".stripMargin)
  }

  @Test
  def testObstructed() {
    assertProgramParsingProduces(List(If(Obstructed(Add(List(GridX(), DeltaX())), Add(List(GridY(), DeltaY()))), List(TurnLeft()), Nil)), "IF (OBSTRUCTED(X+DX,Y+DY)) { LEFT }")
  }

  @Test
  def testNegativeExpression() {
    assertProgramParsingProduces(List(Print(List(StringConstant("-DX = "), Negate(DeltaX())))), """PRINT "-DX = " . -DX""")
  }

  @Test
  def testEvalParameters() {
    assertProgramParsingProduces(List(Push(EvalParam(Constant(1)))), "PUSH $1")
    assertProgramParsingProduces(List(Push(EvalParam(Constant(1)))), "PUSH $(1)")
    assertProgramParsingProduces(List(Push(EvalParam(Add(List(Constant(1), Constant(2), Constant(3)))))), "PUSH $(1 + 2 + 3)")
  }

  @Test
  def testFunc() {
    assertProgramParsingProduces(List(Func("TEST", Add(List(DeltaX(), GridX())))), "FUNC TEST ( DX + X )")
    assertProgramParsingProduces(List(Func("TEST", Add(List(DeltaX(), GridX())))), "FUNCTION TEST ( DX + X )")
  }

  @Test
  def testPred() {
    assertProgramParsingProduces(List(Pred("DY_EQUALS", Equal(DeltaY(), EvalParam(Constant(1))))), "PRED DY_EQUALS ( DY = $1 )")
    assertProgramParsingProduces(List(Pred("DY_EQUALS", Equal(DeltaY(), EvalParam(Constant(1))))), "PREDICATE DY_EQUALS ( DY = $1 )")
  }

  @Test
  def testInvokeFunc() {
    assertProgramParsingProduces(List(Push(InvokeFunc("TEST", List(Constant(42))))), "PUSH TEST(42)")
  }

  @Test
  def testInvokePred() {
    assertProgramParsingProduces(List(If(InvokePred("DY_EQUALS", List(Constant(2))), Nil, Nil)), "IF DY_EQUALS(2) { }")
  }

  @Test
  def testTernary() {
    assertProgramParsingProduces(List(Push(Ternary(Equal(DeltaY(), Constant(0)), DeltaX(), Constant(-1)))), "PUSH ((DY = 0) ? DX : -1)")
  }

  @Test
  def testRepeat() {
    assertProgramParsingProduces(List(Repeat(Constant(5), List(Forward(), TurnRight()))), "REPEAT 5 { FORWARD RIGHT }")
  }
}
