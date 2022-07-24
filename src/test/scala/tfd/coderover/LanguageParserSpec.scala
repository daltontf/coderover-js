// package tfd.coderover

// import wvlet.airspec._

// class LanguageParserSpec extends Specification with DataTables {
//   private[this] val languageParser = new LanguageParser();

//   def parsingListProducesExpression(parser:languageParser.Parser[_])(codeList:List[String], expectedAst:Expression) {
//     val f = parsingProducesExpression(parser) _
//     for (code <- codeList) {
//       f(code, expectedAst)
//     }
//   }

//   def parsingProducesExpression(parser:languageParser.Parser[_])(code:String, expectedAst:Expression) {
//       val parseResult = languageParser.parseAll(parser, code)
//       parseResult.successful mustEqual true
//       parseResult.get mustEqual expectedAst
//   }

//   "Parsing Integer expression" should {
//     "handle superfluous parens" in {
//       "code list"                                                      | "expression" |>
//       List("1", "01", "(1)", "((1))", "(((01)))", "((0000001))")       ! Constant(1)  |
//       List("-1", "-01", "(-1)", "((-01))", "((-00001))")               ! Constant(-1) |
//       parsingListProducesExpression(languageParser.intExpression) _
//     }
//   }

//   "Parsing with parsers" should {
//       "parse comparisons" in {
//       "code list"                                                       |  "expression"                                 |>
//       List( "1 = 2", "(1) = 2", "1 = (2)", "(1) = (2)")                 !  Equal(Constant(1), Constant(2))              |
//       List("-1 < 1")                                                    !  LessThan(Constant(-1), Constant(1))          |
//       List(" 3 >-2")                                                    !  GreaterThan(Constant(3), Constant(-2))       |
//       List(" 3 >= 2")                                                   !  GreaterThanOrEqual(Constant(3), Constant(2)) |
//       List("-4 <= -9")                                                  !  LessThanOrEqual(Constant(-4), Constant(-9))  |
//       List(" 7 <> 3")                                                   !  NotEqual(Constant(7), Constant(3))           |
//       parsingListProducesExpression(languageParser.comparison) _
//     }
//   }

//   def parsingWholeProgramProducesExpression(code:String, expectedAst:List[Instruction]) {
//       val parseResult = languageParser.parse(code)
//       parseResult.successful must == (true)
//       parseResult.get mustEqual expectedAst
//   }


//   "Parsing program" should {
//     "parse empty file" in {
//       for (code <- List("", " ","\n", "\n \n")) parsingWholeProgramProducesExpression(code, List())
//     }

//     "parse COUNT function" in {
//       parsingWholeProgramProducesExpression("PUSH COUNT(FOO)",  List(Push(Count("FOO"))))
//     }

//     "parse ParamCount" in {
//       parsingWholeProgramProducesExpression("PUSH $COUNT", List(Push(ParamCount())))
//     }

//     "should handle functions that begin with builtin name" in {
//       for (builtin <- List(
//         ("PAINT", Paint()),
//         ("FORWARD", Forward()),
//         ("RIGHT", TurnRight()),
//         ("LEFT", TurnLeft()),
//         ("POP", Pop())
//       )) {
//         parsingWholeProgramProducesExpression(("""
//         |PROC """ + builtin._1 + """IT { PRINT "FOO" }
//         |""" + builtin._1 + """IT
//         |""" + builtin._1)
//           .stripMargin, List(Proc(builtin._1 + "IT", List(Print(List(StringConstant("FOO"))))), InvokeProc(builtin._1 + "IT", List()), builtin._2)
//         )
//       }

//       parsingWholeProgramProducesExpression("""
//           |PROC PUSHIT  { PUSH($1) }
//           |PROC PUSHIT2 { PUSH($1) }
//           |PUSHIT(1)
//           |PUSHIT2(1)
//         """.stripMargin, List(
//               Proc("PUSHIT", List(Push(EvalParam(Constant(1))))),
//               Proc("PUSHIT2", List(Push(EvalParam(Constant(1))))),
//               InvokeProc("PUSHIT", List(Constant(1))),
//               InvokeProc("PUSHIT2", List(Constant(1))))
//       )

//       parsingWholeProgramProducesExpression("""
//           |PROC REPLACEIT  { REPLACE($1) }
//           |REPLACEIT(1)
//         """.stripMargin, List(
//               Proc("REPLACEIT", List(Replace(EvalParam(Constant(1))))),
//               InvokeProc("REPLACEIT", List(Constant(1))))
//       )

//       parsingWholeProgramProducesExpression("""
//           |PROC STOREIT  { STORE($1,$2) }
//           |STOREIT(1,2)
//         """.stripMargin, List(
//               Proc("STOREIT", List(Store(EvalParam(Constant(1)), EvalParam(Constant(2))))),
//               InvokeProc("STOREIT", List(Constant(1), Constant(2))))
//       )
//     }
//   }
// }