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
    namedExpressions map ( exp => exp._1 -> Signal(eval(getReferenceExpr(exp._1, namedExpressions), namedExpressions)) )
  }


  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(name) => {
      val ref = getReferenceExpr(name, references)
      eval(ref, references - name)
    }
    case Plus(a, b) => operation(a, b, references)(_ + _)
    case Minus(a, b) => operation(a, b, references)(_ - _)
    case Times(a, b) => operation(a, b, references)(_ * _)
    case Divide(a, b) => {
      val valExp = eval(b, references)
      if (valExp > 0) eval(a, references) / valExp
      else Double.NaN
    }
    case _ => Double.NaN
  }

  private def operation(a: Expr, b: Expr, references: Map[String, Signal[Expr]])(f: (Double, Double) => Double) = {
    val aVal = eval(a, references)
    val bVal = eval(b, references)
    f(aVal,bVal)
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
