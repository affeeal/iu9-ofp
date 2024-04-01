enum BinaryOp:
  case Add, Mul

abstract class Expr
case class VarExpr(name: String) extends Expr
case class BinaryExpr(op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr
case class LetExpr(v: VarExpr, subst: Expr, to: Expr) extends Expr

object VarExpr {
  private val base = "v";
  private var counter = -1;

  def nextName(): String = {
    counter += 1
    base + counter
  }
}

object Main {
  private def occurenciesOf(what: VarExpr, where: Expr): Int =
    where match {
      case `what` => 1
      case BinaryExpr(_, lhs, rhs) => {
        occurenciesOf(what, lhs) + occurenciesOf(what, rhs)
      }
      case LetExpr(_, subst, to) => {
        occurenciesOf(what, subst) + occurenciesOf(what, to)
      }
      case other => 0
    }

  private def replace(what: VarExpr, by: Expr, where: Expr): Expr =
    where match {
      case `what` => by
      case BinaryExpr(op, lhs, rhs) => {
        BinaryExpr(op, replace(what, by, lhs), replace(what, by, rhs))
      }
      case LetExpr(v, subst, to) => {
        LetExpr(v, replace(what, by, subst), replace(what, by, to))
      }
      case other => other
    }

  def letsOptimize(what: Expr): Expr =
    what match {
      case BinaryExpr(op, lhs, rhs) if lhs == rhs => {
        val v = VarExpr(VarExpr.nextName())
        LetExpr(v, letsOptimize(lhs), BinaryExpr(op, v, v))
      }
      case BinaryExpr(op, lhs, rhs) => {
        BinaryExpr(op, letsOptimize(lhs), letsOptimize(rhs))
      }
      case LetExpr(v, subst, to) if occurenciesOf(v, to) == 1 => {
        letsOptimize(replace(v, subst, to))
      }
      case LetExpr(v, subst, to) => {
        LetExpr(v, letsOptimize(subst), letsOptimize(to))
      }
      case other => other
    }

  def main(args: Array[String]): Unit = {
    // Вынесение общих подвыражений:
    // (x + y) * (x + y)
    val ce1 = BinaryExpr(BinaryOp.Add, VarExpr("x"), VarExpr("y"))
    val ce2 = BinaryExpr(BinaryOp.Mul, ce1, ce1)
    println(letsOptimize(ce2))

    // ((x + y) * (x + y) + (x + y) * (x + y)) + z
    val ce3 = BinaryExpr(BinaryOp.Add, ce2, ce2)
    val ce4 = BinaryExpr(BinaryOp.Add, ce3, VarExpr("z"))
    println(letsOptimize(ce4))

    // Удаление единственной let-переменной:
    // let x = (y + z) in a * x
    val re1 = BinaryExpr(BinaryOp.Add, VarExpr("y"), VarExpr("z"))
    val re2 = BinaryExpr(BinaryOp.Mul, VarExpr("a"), VarExpr("x"))
    val re3 = LetExpr(VarExpr("x"), re1, re2)
    println(letsOptimize(re3))

    // let y = (c + d) * (c + d) in (let x = (y + z) in a * x)
    val re4 = BinaryExpr(BinaryOp.Add, VarExpr("c"), VarExpr("d"))
    val re5 = BinaryExpr(BinaryOp.Mul, re4, re4)
    val re6 = LetExpr(VarExpr("y"), re5, re3)
    println(letsOptimize(re6))
  }
}
