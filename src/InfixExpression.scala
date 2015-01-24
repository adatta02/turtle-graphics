abstract sealed class InfixElement {
  val precedence: Int = -1  
}

case class Operand(value: String) extends InfixElement {
  
  def getValue(context: Option[Map[String, Double]]):Double = {
    
    if(context.isEmpty){
      value.toDouble
    }else{
      context.get.getOrElse(value, value.toDouble)
    }
    
  }
  
}

case class LeftParen extends InfixElement
case class RightParen extends InfixElement

abstract sealed class Operator extends InfixElement {
  def apply(operands: List[Operand], context: Option[Map[String, Double]]):Double
}

case class Mult extends Operator {
  override val precedence: Int = 3
  def apply(operands: List[Operand], context: Option[Map[String, Double]]):Double = {
    operands.drop(1).foldLeft(operands.head.getValue(context))((total, el) => {
      total * el.getValue(context)
    })
  }
}

case class Div extends Operator {
  override val precedence: Int = 3
  def apply(operands: List[Operand], context: Option[Map[String, Double]]):Double = {
    operands.drop(1).foldLeft(operands.head.getValue(context))((total, el) => {
      total / el.getValue(context)
    })    
  }  
}

case class Plus extends Operator {
  override val precedence: Int = 2
  def apply(operands: List[Operand], context: Option[Map[String, Double]]):Double = {
    operands.drop(1).foldLeft(operands.head.getValue(context))((total, el) => {
      total + el.getValue(context)
    })    
  }  
}

case class Minus extends Operator {
  override val precedence: Int = 2
  def apply(operands: List[Operand], context: Option[Map[String, Double]]):Double = {
    operands.drop(1).foldLeft(operands.head.getValue(context))((total, el) => {
      total - el.getValue(context)
    })    
  }  
}

case class InfixExpression(parsedExpression: List[InfixElement]){
  
  // http://en.wikipedia.org/wiki/Shunting-yard_algorithm
  def convertToRPN: List[InfixElement] = {    
    
    val result = parsedExpression.foldLeft( (List[InfixElement](), List[InfixElement]()) )((stacks, el) => {
    	
    	val (stack, output) = el match {

	    	case LeftParen() => ( stacks._1 :+ el, stacks._2 )
	    	
	    	case RightParen() => {
	    	  
	    	  val (poppedOperators, newStack) = stacks._1.reverse.span(a => {
	    	    a match {
	    	      case LeftParen() => false
	    	      case _ => true
	    	    }
	    	  })
	    	  
	    	  ( newStack.drop(1).reverse, stacks._2 ::: poppedOperators )
	    	}
	
	    	case op: Operand => ( stacks._1, stacks._2 :+ el )
	
	    	case Plus() | Minus() | Div() | Mult() => {
	    	  
	    		if( stacks._1.length > 0 && el.precedence <= stacks._1.last.precedence ){
	    			val (poppedOperators, newStack) = stacks._1.reverse.span(a => el.precedence <= a.precedence)
	    			( newStack.reverse :+ el , stacks._2 ::: poppedOperators )
	    		}else{    		  				  
				  (stacks._1 :+ el, stacks._2)  
	    		}
	    		
	    	}
	
	    	case _ => ( stacks._1, stacks._2 )
    	}
	  	  
      (stack, output)
    })            
        
    result._2 ::: result._1.reverse
  }
  
  def evaluate(context: Option[Map[String, Double]]): Double = {
    val rpn = this.convertToRPN
            
    val evaluatedStack = 
      rpn.foldLeft(List[InfixElement]())((stack, el) => {
      
      val newStack = el match {
        case v: Operand => {
          el +: stack
        }
        case Plus() | Minus() | Div() | Mult() => {
          val (calcInputs, modStack) = stack.splitAt(2)
          val calcResult = el.asInstanceOf[Operator](calcInputs.reverse.map(_.asInstanceOf[Operand]), context)
          new Operand(calcResult.toString) +: modStack          
        }        
        case _ => stack
      }
      
      newStack
    })
    
    evaluatedStack.head.asInstanceOf[Operand].getValue(context)
  }
}
