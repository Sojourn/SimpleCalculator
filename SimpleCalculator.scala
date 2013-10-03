import scala.annotation.tailrec

abstract class Operator {
  def apply(operand1: Int, operand2: Int): Int
}

object PlusOperator extends Operator {
  def apply(operand1: Int, operand2: Int) = operand1 + operand2
}

object MinusOperator extends Operator {
  def apply(operand1: Int, operand2: Int) = operand1 - operand2
}

case class CalculatorContext(
  state: CalculatorState,
  operand: Int,
  operator: Operator)

abstract class Token
class NumberToken(val value: Int) extends Token
class OperatorToken extends Token
class PlusToken extends OperatorToken
class MinusToken extends OperatorToken

abstract class CalculatorState {
  def apply(token: Token, context: CalculatorContext): Option[CalculatorContext]
}

object ExpectFirstOperand extends CalculatorState {
  def apply(token: Token, context: CalculatorContext): Option[CalculatorContext] = {
    token match {
      case number: NumberToken => Some(CalculatorContext(
        ExpectOperator,
        number.value,
        context.operator))

      case operator: OperatorToken => None
    }
  }
}

object ExpectSecondOperand extends CalculatorState {
  def apply(token: Token, context: CalculatorContext): Option[CalculatorContext] = {
    token match {
      case number: NumberToken => Some(CalculatorContext(
        ExpectOperator,
        context.operator(context.operand, number.value),
        context.operator))

      case operator: OperatorToken => None
    }
  }
}

object ExpectOperator extends CalculatorState {
  def parseToken(token: Token): Operator = {
    token match {
      case plus: PlusToken => PlusOperator
      case minus: MinusToken => MinusOperator
    }
  }

  def apply(token: Token, context: CalculatorContext): Option[CalculatorContext] = {
    token match {
      case number: NumberToken => None

      case operator: OperatorToken => {
        Some(CalculatorContext(ExpectSecondOperand, context.operand, parseToken(token)))
      }
    }
  }
}

object Calculator {
  def eval(expression: List[Token]): Option[Int] = {

    @tailrec
    def evalIter(expression: List[Token], context: CalculatorContext): Option[CalculatorContext] = {
      expression match {
        case Nil => Some(context)

        case head :: tail => context.state(head, context) match {
          case Some(nextContext) => evalIter(tail, nextContext)
          case None => None
        }
      }
    }

    evalIter(expression, CalculatorContext(ExpectFirstOperand, 0, null)) match {
      case Some(context) => context.state match {
        case ExpectOperator => Some(context.operand)
        case _ => None
      }
      case None => None
    }
  }

  def main(args: Array[String]) {
    val expression = List(
      new NumberToken(1),
      new PlusToken,
      new NumberToken(2),
      new MinusToken,
      new NumberToken(2),
      new MinusToken,
      new NumberToken(3))

    println(Calculator.eval(expression))
  }
}