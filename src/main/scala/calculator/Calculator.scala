package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (name, signal) => {
        //val expr = signal()

        (name, Signal(eval(signal(), namedExpressions)))
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def isCyclicExpr(callers: Set[String], expr: Expr): Boolean = {
      expr match {
        case Literal(v) => false
        case Ref(name) =>
          if(callers.contains(name)) true
          else isCyclicExpr(callers + name, getReferenceExpr(name, references))
        case Plus(a,b) => isCyclicExpr(callers, a) || isCyclicExpr(callers, b)
        case Minus(a,b) => isCyclicExpr(callers, a) || isCyclicExpr(callers, b)
        case Times(a,b) => isCyclicExpr(callers, a) || isCyclicExpr(callers, b)
        case Divide(a,b) => isCyclicExpr(callers, a) || isCyclicExpr(callers, b)
      }
    }

    expr match {
      case Ref(name) =>
        if (isCyclicExpr(Set(name), getReferenceExpr(name, references))) Double.NaN
        else eval(getReferenceExpr(name, references), references)
      case Literal(v) => v
      case Plus(a,b) => eval(a, references) + eval(b, references)
      case Minus(a,b) => eval(a, references) - eval(b, references)
      case Times(a,b) => eval(a, references) * eval(b, references)
      case Divide(a,b) => eval(a, references) / eval (b, references)
    }
  }



  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
