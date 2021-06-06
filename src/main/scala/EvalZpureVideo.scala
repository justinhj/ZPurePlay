import zio.prelude._
import zio.prelude.fx.ZPure
import scala.math.{Numeric => _}

object EvalZPureVideo {

  import EvalZPureVideo._

  // Our Numeric
  object Numeric {
    // This can be used to summon a numeric (same as the library function `implicitly`)
    def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric

    object ops {

      implicit class NumericOps[T](a: T)(implicit n: Numeric[T]) {
        def add(b: T): T = n.add(a, b)
        def +(b: T): T = n.add(a, b)

        def mul(b: T): T = n.mul(a, b)
        def *(b: T): T = n.mul(a, b)

        def sub(b: T): T = n.sub(a, b)
        def -(b: T): T = n.sub(a, b)

        def div(b: T): T = n.div(a, b)
        def /(b: T): T = n.div(a, b)
      }
    }
  }

  trait Numeric[T] {
    def add(a: T, b: T): T
    def sub(a: T, b: T): T
    def div(a: T, b: T): T
    def mul(a: T, b: T): T
  }

  implicit val numericInt: Numeric[Int] = new Numeric[Int] {
    def add(a: Int, b: Int): Int = a + b
    def sub(a: Int, b: Int): Int = a - b
    def div(a: Int, b: Int): Int = a / b
    def mul(a: Int, b: Int): Int = a * b
  }

  implicit val numericLong: Numeric[Long] = new Numeric[Long] {
    def add(a: Long, b: Long): Long = a + b
    def mul(a: Long, b: Long): Long = a * b
    def sub(a: Long, b: Long): Long = a - b
    def div(a: Long, b: Long): Long = a / b
  }

  // Error type (for errors that occur evaluating expressions)
  sealed trait Error
  object SymbolNotFound extends Error
  object DivisionByZero extends Error

  // Log type is just a list of strings
  type Log = String

  // Environment type (our symbol table for lookups)
  type Env[A] = Map[String, A]

  // Here's an ADT (abstract data type) for our expression evaluator
  sealed trait Exp[A]
  case class Val[A](value: A) extends Exp[A]
  case class Add[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Sub[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Mul[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Div[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Var[A](identifier: String) extends Exp[A]

  type Result[A] = ZPure[Log, Any, Any, Env[A], Error, A]

  import Numeric.ops._

  implicit def numericZResult[A: Numeric]: Numeric[Result[A]] = new Numeric[Result[A]] {
    def add(x: Result[A], y: Result[A]): Result[A] = {
      x.zip(y).flatMap {
        case (a,b) =>
          val c = a + b
          ZPure.succeed(c).log(s"Add $a and $b and got $c")
      }
    }

    def mul(x: Result[A], y: Result[A]): Result[A] = {
        x.zip(y).flatMap {
        case (a,b) =>
          val c = a * b
          ZPure.succeed(c).log(s"Mul $a and $b and got $c")
      }
    }

    def sub(x: Result[A], y: Result[A]): Result[A] = {
      x.zip(y).flatMap {
        case (a,b) =>
          val c = a - b
          ZPure.succeed(c).log(s"Sub $b from $a and got $c")
      }
    }

    def div(x: Result[A], y: Result[A]): Result[A] = {
      x.zip(y).flatMap {
        case (a,b) =>
          val c = a / b
          ZPure.succeed(c).log(s"Div $a by $b and got $c")
      }
    }
  }

  // Evaluator
  def eval[A: Numeric](exp: Exp[A]): Result[A] =
    exp match {
      case Var(id)    => handleVar(id)
      case Val(value) => ZPure.succeed(value).log(s"Literal value $value")
      case Add(l, r)  => handleAdd(l, r)
      case Sub(l, r)  => handleSub(l, r)
      case Mul(l, r)  => handleMul(l, r)
      case Div(l, r)  => handleDiv(l, r)
    }

  def handleVar[A: Numeric](s: String): Result[A] = {
    ZPure.environment[Any,Env[A]].flatMap {
      env =>
        ZPure.fromOption(env.get(s)).mapError(_ => SymbolNotFound).
        flatMap {
          a =>
            ZPure.log(s"Var $s value $a").as(a)
        }
    }
  }

  def handleAdd[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) + eval(r)
  def handleMul[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) * eval(r)
  def handleDiv[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) / eval(r)
  def handleSub[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) - eval(r)

  def main(args: Array[String]): Unit = {

    val env1: Env[Int] = Map("x" -> 1, "y" -> 10, "z" -> 100)

    val exp1 = Add(
                Mul(
                  Mul(
                    Val(10),
                    Var("x")),
                  Var("y")
                ),
                Var("z"))

    val exp2 = Val(10)
    val exp3 = Var[Int]("x")

    val eval2 = eval(exp1).provide(env1).runAll()

    println(s"result ${eval2}")
    eval2._1.foreach {
      l => println(l)
    }

  }
}
