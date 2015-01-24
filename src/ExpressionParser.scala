import scala.util.parsing.combinator._

// Grammar from 
// http://stackoverflow.com/questions/11533547/operator-precedence-with-scala-parser-combinators

object ExpressionParser extends RegexParsers {
  
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
    """-?(\d+)|(\w+)""".r ^^ ( new Operand(_) )
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
  
  def main(args: Array[String]):Unit = {
    val input = """xPos + ((1 + 2) * 4) - 3"""
    val res = parseAll(infixExpr, input)
    val context = Option[Map[String, Double]](Map("xPos" -> 8.0))    
    
    println(res.get.evaluate(context))
  }
  
}