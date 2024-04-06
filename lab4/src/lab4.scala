enum BinaryOp:
  case Plus, Star

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
  def occurenciesOf(what: VarExpr, where: Expr): Int = where match {
      case `what` => 1
      case BinaryExpr(_, lhs, rhs) => {
        occurenciesOf(what, lhs) + occurenciesOf(what, rhs)
      }
      case LetExpr(_, subst, to) => {
        occurenciesOf(what, subst) + occurenciesOf(what, to)
      }
      case other => 0
    }

  def replace(what: VarExpr, by: Expr, where: Expr): Expr = where match {
      case `what` => by
      case BinaryExpr(op, lhs, rhs) => {
        BinaryExpr(op, replace(what, by, lhs), replace(what, by, rhs))
      }
      case LetExpr(v, subst, to) => {
        LetExpr(v, replace(what, by, subst), replace(what, by, to))
      }
      case other => other
    }

  def letsOptimize(what: Expr): Expr = what match {
      case BinaryExpr(op, lhs @ BinaryExpr(_, _, _), rhs) if lhs == rhs => {
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
    // x + x → x + x
    val e1 = BinaryExpr(BinaryOp.Plus, VarExpr("x"), VarExpr("x"))
    println(letsOptimize(e1))
  
    // (x + y) * (x + y) → let v0 = x + y in v0 * v0
    val e2 = BinaryExpr(BinaryOp.Plus, VarExpr("x"), VarExpr("y"))
    val e3 = BinaryExpr(BinaryOp.Star, e2, e2)
    println(letsOptimize(e3))

    // ((x + y) * (x + y) + (x + y) * (x + y)) * z →
    // (let v0 = (x + y) * (x + y) in v0 + v0) * z →
    // (let v0 = (let v1 = x + y in v1 * v1) in v0 + v0) * z
    val e4 = BinaryExpr(BinaryOp.Plus, e3, e3)
    val e5 = BinaryExpr(BinaryOp.Star, e4, VarExpr("z"))
    println(letsOptimize(e5))

    // let x = y + z in a * x → a * (y + z)
    val e6 = BinaryExpr(BinaryOp.Plus, VarExpr("y"), VarExpr("z"))
    val e7 = BinaryExpr(BinaryOp.Star, VarExpr("a"), VarExpr("x"))
    val e8 = LetExpr(VarExpr("x"), e6, e7)
    println(letsOptimize(e8))

    // let y = (c + d) * (c + d) in (let x = y + z in a * x) →
    // let x = (c + d) * (c + d) + z in a * x →
    // a * ((c + d) * (c + d) + z) →
    // a * ((let v0 = c + d in v0 * vo) + z)
    val e9 = BinaryExpr(BinaryOp.Plus, VarExpr("c"), VarExpr("d"))
    val e10 = BinaryExpr(BinaryOp.Star, e9, e9)
    val e11 = LetExpr(VarExpr("y"), e10, e8)
    println(letsOptimize(e11))
  }
}
