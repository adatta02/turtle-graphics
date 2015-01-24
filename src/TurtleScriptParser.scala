import scala.util.parsing.combinator._

case class Turtle(position: (Int, Int) = (0, 0), penDown: Boolean = false, pen: String = "#000000") {
  def getContext: Map[String, Double] = {
    Map("xPos" -> position._1, "yPos" -> position._2)
  }
}

sealed trait Expr {  
  def updateTurtle(turtle: Turtle): List[Turtle] = {
    List(turtle)
  }  
}

case class Repeat(num: Int, steps: List[Expr]) extends Expr {
  
  def evalSteps(currentTurtle: Turtle, stepList: List[Expr]): List[Turtle] = {
    
    steps.foldLeft(List[Turtle](currentTurtle))((list, step) => {
      list ::: step.updateTurtle(list.last)
    }).drop(1)
    
  }
  
  override def updateTurtle(turtle: Turtle): List[Turtle] = {
        
    (1 to num)
      .toList.map(a => steps)
      .foldLeft(List[Turtle](turtle))((list, steps) => {        
        list ::: this.evalSteps(list.last, steps)
	}).drop(1)
	
  }
  
}

case class Move(direction: String, amountExpr:InfixExpression) extends Expr {
  
  override def updateTurtle(turtle: Turtle): List[Turtle] = {
    
    val amount = amountExpr.evaluate(Option[Map[String, Double]](turtle.getContext)).toInt
    
    val newPos = direction match {
      case "left" => (turtle.position._1 - amount, turtle.position._2)
      case "right" => (turtle.position._1 + amount, turtle.position._2)
      case "up" => (turtle.position._1, turtle.position._2 - amount)
      case "down" => (turtle.position._1, turtle.position._2 + amount)      
      case _ => turtle.position
    }
    
    List(Turtle(newPos, turtle.penDown, turtle.pen))
  }
  
}

case class Setpos(positionExpr: (InfixExpression, InfixExpression)) extends Expr {
  override def updateTurtle(turtle: Turtle): List[Turtle] = {
    val context = Option[Map[String, Double]](turtle.getContext)
    val position = (positionExpr._1.evaluate(context).toInt, positionExpr._2.evaluate(context).toInt)
    
    List(Turtle(position, turtle.penDown, turtle.pen))
  }
}

case class Setpen(down: Boolean) extends Expr {
  override def updateTurtle(turtle: Turtle): List[Turtle] = {
    List(Turtle(turtle.position, down, turtle.pen))
  }
}

object TurtleScriptParser extends RegexParsers { 
  
  def infixExpr: Parser[ InfixExpression ] = {
    expr ^^ ( new InfixExpression(_) )
  }
  
  def expr: Parser[ List[InfixElement] ] = {
    term ~ rep(plusMin) ^^ (f => {
      f._1 ::: f._2.flatten
    })
  }
  
  def term: Parser[ List[InfixElement] ] = {
    factor ~ rep(multDiv) ^^ (f => {
      f._1 ::: f._2.flatten
    })
  }
  
  def factor: Parser[ List[InfixElement] ] = {    
    (number | literal("(") ~> expr <~ literal(")")) ^^ (f => {
      
      f match {
        case num: InfixElement => List(num)
        case expr: List[InfixElement] => new LeftParen +: expr :+ new RightParen        
        case _ => throw new Exception("unexpected type")
      }
      
    })
  }  
  
  def number: Parser[InfixElement] = { 
    """-?(\.?\d+)|(xPos)|(yPos)""".r ^^ ( new Operand(_) )
  }
  
  def plusMin: Parser[ List[InfixElement] ] = {
    (literal("+") | literal("-")) ~ term ^^ (f => {      
      val op = f._1 match {
        case "+" => new Plus
        case "-" => new Minus
      }      
      List(op) ::: f._2
    })
  }
    
  def multDiv: Parser[ List[InfixElement] ] = {
    (literal("/") | literal("*")) ~ factor ^^ (f => {
      val op = f._1 match {
        case "/" => new Div
        case "*" => new Mult
      }      
      List(op) ::: f._2
    })
  }  
    
  def move: Parser[Move] = {
    ((literal("up") | literal("down") | literal("right") | literal("left")) ~ infixExpr) ^^ (comboParser => {      
      Move(comboParser._1, comboParser._2)
    })    
  }      
  
  def setpos: Parser[Setpos] = {
    (literal("setpos") ~> infixExpr ~ infixExpr) ^^ (comboParser => {
      Setpos(comboParser._1, comboParser._2)
    })
  }
  
  def pendown: Parser[Setpen] = {
    literal("pendown") ^^ (v => Setpen(true))
  }  
  
  def penup: Parser[Setpen] = {
    literal("penup") ^^ (v => Setpen(false))
  }  
  
  def home: Parser[Setpos] = {
    literal("home") ^^ (v => {
      val zeroExpr = new InfixExpression( List(new Operand("0")) )
      Setpos(zeroExpr, zeroExpr)
    })
  }
  
  def exprList: Parser[ List[Expr] ] = {
    (move | setpos | home | pendown | penup)*
  }
  
  def rangeExprList: Parser[ List[Expr] ] = {
    literal("[") ~> exprList <~ literal("]")
  }
  
  def repeat: Parser[Repeat] = {
    literal("repeat") ~> """(\d+)""".r ~ rangeExprList ^^ (comboParser => {      
      Repeat(comboParser._1.toInt, comboParser._2)
    })
  }
  
  def parseTurtleExpressions: Parser[ List[Expr] ] = {
    (move | setpos | home | pendown | penup | repeat)*
  }     
  
}