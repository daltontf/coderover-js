package tfd.coderover

import scala.util.parsing.combinator.JavaTokenParsers

import scala.scalajs.js.annotation._

@JSExportTopLevel("LanguageParser")
class LanguageParser extends JavaTokenParsers {

  override protected val whiteSpace = """(\s+|//[^\n]*\n|/\*(.|[\r\n])*?\*/|//[^\n]*$)+""".r

  lazy val constant:Parser[Constant] = wholeNumber ^^ { x => Constant(x.toInt) }

  override def skipWhitespace = true

  def onlyKeyword(s: String): Parser[String] = new Parser[String] {
	    def apply(in: Input) = {
	      val source = in.source
	      val offset = in.offset
	      val start = handleWhiteSpace(source, offset)
	      var i = 0
	      var j = start
	      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
	        i += 1
	        j += 1
	      }
	      if (i == s.length && (j == source.length() || !(source.charAt(j).isLetterOrDigit || source.charAt(j) == '_')))
	        Success(source.subSequence(start, j).toString, in.drop(j - offset))
	      else  {
	        val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
	        Failure("`"+s+"' expected but "+found+" found", in.drop(start - offset))
	      }
	    }
	  }

  lazy val intExpression:Parser[IntExpression] = parenIntExpression |
    evalParam |
    constant |
    ternaryExpression |
    arityNoneIntFunction |
    arityOneIntFunction |
    arityTwoIntFunction |
    entityIndexFunction |
    countEntity |
    negatedIntExpression |
    invokeFunc

  lazy val parenIntExpression:Parser[IntExpression] = "(" ~> ( mathematical | intExpression) <~ ")"

  lazy val negatedIntExpression:Parser[IntExpression] = "-"~>(intExpression) ^^ { expression => Negate(expression) }

  lazy val invokeFunc:Parser[InvokeFunc] = ident ~ opt(params)  ^^ {
    case defName~Some(x) => InvokeFunc(defName, x)
    case defName~None => InvokeFunc(defName, Nil)
  }

  lazy val mathematical:Parser[Mathematical] = mathematical("+", (head:IntExpression, tail:List[IntExpression]) => Add(head :: tail)) |
    mathematical("-", (head:IntExpression, tail:List[IntExpression]) => Subtract(head :: tail)) |
    mathematical("*", (head:IntExpression, tail:List[IntExpression]) => Multiply(head :: tail)) |
    mathematical("/", (head:IntExpression, tail:List[IntExpression]) => Divide(head :: tail)) |
    mathematical("%", (head:IntExpression, tail:List[IntExpression]) => Modulus(head :: tail))

  def mathematical(sign:String, f:(IntExpression, List[IntExpression]) => Mathematical) =
    (intExpression ~ sign ~ rep1sep(intExpression, sign)) ^^ {
      case head~sign~tail => f(head, tail)
    }

  lazy val expressionParameter:Parser[IntExpression] = mathematical | intExpression

  lazy val ternaryExpression:Parser[Ternary] =  booleanExpression ~ "?" ~ expressionParameter ~ ":" ~ expressionParameter ^^ {
    case exp~_~thenEx~_~elseEx => Ternary(exp, thenEx, elseEx)
  }

  lazy val arityNoneIntFunction:Parser[IntExpression] = ("TOP"|"X"|"Y"|"DX"|"DY"|"DEPTH"|"$COUNT") ^^ {
    case "TOP" => Top()
    case "X" => GridX()
    case "Y" => GridY()
    case "DX" => DeltaX()
    case "DY" => DeltaY()
    case "DEPTH" => Depth()
    case "$COUNT" => ParamCount()
  }

  lazy val arityOneIntFunction:Parser[IntExpression] = ("ABS"|"MEM") ~ "(" ~ expressionParameter <~ ")" ^^ {
    case "ABS"~_~parm => Abs(parm)
    case "MEM"~_~parm => Mem(parm)
  }

  lazy val arityTwoIntFunction:Parser[IntExpression] = ("MAX"|"MIN") ~ "(" ~ expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
    case "MAX"~_~parm1~_~parm2 => Max(parm1, parm2)
    case "MIN"~_~parm1~_~parm2 => Min(parm1, parm2)
  }

  lazy val entityIndexFunction:Parser[IntExpression] = ("DISTANCEX" | "DISTANCEY" ) ~ "(" ~ ident ~ opt(parenIntExpression) <~ ")" ^^ {
    case "DISTANCEX"~_~parm~index => DistanceX(parm, index.getOrElse(Constant(1)))
    case "DISTANCEY"~_~parm~index => DistanceY(parm, index.getOrElse(Constant(1)))
  }

  lazy val countEntity  = "COUNT" ~ "(" ~> ident <~ ")" ^^ { Count(_) }

  lazy val evalParam:Parser[EvalParam] = """\$""".r ~> (constant | parenIntExpression) ^^ { x => EvalParam(x) }

  lazy val arityTwoBoolean:Parser[BooleanExpression] = ("PAINTED"|"OBSTRUCTED") ~ "(" ~ expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
    case "PAINTED"~_~x~_~y => Painted(x, y)
    case "OBSTRUCTED"~_~x~_~y => Obstructed(x, y)
  }

  lazy val comparison:Parser[Comparison] = intExpression ~ ("=" | "<=" | ">=" | "<>" | "<" | ">" ) ~ intExpression ^^ {
    case left~"="~right 	=> Equal(left, right)
    case left~"<="~right 	=> LessThanOrEqual(left, right)
    case left~">="~right 	=> GreaterThanOrEqual(left, right)
    case left~"<>"~right 	=> NotEqual(left, right)
    case left~"<"~right 	=> LessThan(left, right)
    case left~">"~right 	=> GreaterThan(left, right)
  }

  lazy val parenBoolean:Parser[BooleanExpression] = "(" ~> (nestedBoolean | booleanExpression) <~ ")"

  lazy val nestedBoolean:Parser[BooleanExpression] = comparison | logical 

  lazy val booleanExpression:Parser[BooleanExpression] =  parenBoolean | not | adjacent | arityTwoBoolean | invokePred

  lazy val not:Parser[BooleanExpression] = "NOT" ~> parenBoolean ^^ { expression => Not(expression) }
  
  lazy val logical:Parser[BooleanExpression] = logicalOr | logicalAnd
  
  lazy val logicalOr = (booleanExpression ~ "OR" ~ rep1sep(booleanExpression, "OR")) ^^ {
    case head~"OR"~tail => Or(head :: tail)
  } 
  
   lazy val logicalAnd = (booleanExpression ~ "AND" ~ rep1sep(booleanExpression, "AND")) ^^ {
    case head~"AND"~tail => And(head :: tail)
  }

  lazy val printIntExpression:Parser[IntExpression] = parenIntExpression |
    evalParam |
    constant |
    ternaryExpression |
    arityNoneIntFunction |
    arityOneIntFunction |
    arityTwoIntFunction |
    entityIndexFunction |
    countEntity |
    negatedIntExpression

  lazy val printBooleanExpression:Parser[BooleanExpression] =  parenBoolean | not | adjacent | arityTwoBoolean
   
  lazy val printString = "PRINT" ~> rep1sep((printIntExpression | printBooleanExpression | stringConstant | invokeFunc | invokePred), ".") ^^ { Print(_) }

  
  lazy val stringConstant = stringLiteral ^^ { x=> StringConstant(x.substring(1, x.length-1)) }
  
  lazy val elseBlock:Parser[List[Instruction]] = "ELSE" ~ "{" ~> rep(instruction) <~ "}"
  
  lazy val elseIfBlock:Parser[List[Instruction]] = "ELSE" ~> ifStatement ^^ {  x => List(x)  }
  
  lazy val ifStatement:Parser[If] = "IF" ~> booleanExpression ~ "{" ~ rep(instruction) ~ "}" ~  opt(elseBlock | elseIfBlock) ^^ {
   	case ifExpression~_~thenInstructions~_~Some(elseInstructions) => If(ifExpression, thenInstructions, elseInstructions)
   	case ifExpression~_~thenInstructions~_~None => If(ifExpression, thenInstructions, Nil)
 	}
  
  lazy val whileStatement:Parser[While] = "WHILE" ~> booleanExpression ~ "{" ~ rep(instruction) <~ "}" ^^ {
   	case whileExpression~_~blockInstructions => While(whileExpression, blockInstructions)
 	}
  
  lazy val forward:Parser[Forward] = onlyKeyword("FORWARD") ^^ { _  => Forward() }

  lazy val pop:Parser[Pop] = onlyKeyword("POP") ^^ { _ => Pop() }
  
  lazy val right:Parser[TurnRight] = onlyKeyword("RIGHT") ^^ { _ => TurnRight() }
  
  lazy val left:Parser[TurnLeft] = onlyKeyword("LEFT") ^^ { _ => TurnLeft() }
  
  lazy val paint:Parser[Paint] = onlyKeyword("PAINT") ^^ { _ => Paint() }
  
  lazy val push:Parser[Push] = onlyKeyword("PUSH") ~> expressionParameter ^^ { x:IntExpression => Push(x)}

  lazy val replace:Parser[Replace] = onlyKeyword("REPLACE")~>expressionParameter ^^ { x:IntExpression => Replace(x) }

  lazy val store:Parser[Store] = "STORE" ~ "(" ~> expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
    case address~_~value => Store(address, value)
  }

  lazy val invokeProc:Parser[InvokeProc] = ident ~ opt(params)  ^^ {
    case defName~Some(x) => InvokeProc(defName, x)
    case defName~None => InvokeProc(defName, Nil)
  }

  lazy val command:Parser[Instruction] = push | replace | store | printString | repeat | forward | right | left | paint | pop | invokeProc
  
  lazy val controlFlow:Parser[Instruction] = ifStatement | whileStatement
  
  lazy val proc:Parser[Proc] = ("PROCEDURE" | "PROC") ~> ident ~ "{" ~ rep(instruction) <~ "}" ^^ {
    case name~_~instructions => Proc(name, instructions)
  }

  lazy val func:Parser[Func] = ("FUNCTION" | "FUNC") ~> ident ~ "(" ~ expressionParameter <~ ")" ^^ {
    case name~_~expression => Func(name, expression)
  }

  lazy val pred:Parser[Pred] = ("PREDICATE" | "PRED") ~> ident ~ booleanExpression ^^ {
    case name~expression => Pred(name, expression)
  }

  lazy val adjacent:Parser[BooleanExpression] = "ADJACENT" ~ "(" ~> ident <~ ")" ^^ { Adjacent(_) }

  lazy val invokePred:Parser[InvokePred] = ident ~ opt(params)  ^^ {
    case defName~Some(x) => InvokePred(defName, x)
    case defName~None => InvokePred(defName, Nil)
  }

  lazy val repeat:Parser[Repeat] = "REPEAT" ~> expressionParameter ~ "{" ~ rep(instruction) <~ "}" ^^ {
    case timesExpression~_~instructions => Repeat(timesExpression, instructions)
  }

  lazy val param:Parser[IntExpression] = mathematical | intExpression

  lazy val params:Parser[List[IntExpression]] = "(" ~> repsep(param, ",") <~ ")"
  
  lazy val instruction:Parser[Instruction] = positioned ( controlFlow | command ) 
  
  def topLevelInstruction:Parser[Instruction] =  proc | func | pred |instruction 
  
  def program = rep(topLevelInstruction)
  
  @JSExport
  def parse(text:String) = parseAll(program, text)

}
