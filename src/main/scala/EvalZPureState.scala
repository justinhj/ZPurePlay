import zio.prelude._
import zio.prelude.fx.ZPure
import scala.math.{Numeric => _}

object EvalZPureState {

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

  // Produce n tabs as a String
  private def nTabs(n: Int): String = List.fill(n)('\t').mkString

  // Here's an ADT (abstract data type) for our expression evaluator
  sealed trait Exp[A]
  case class Val[A](value: A) extends Exp[A]
  case class Add[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Sub[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Mul[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Div[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Var[A](identifier: String) extends Exp[A]

  type Result[A] = ZPure[Log, Int, Int, Env[A], Error, A]

  import Numeric.ops._

  implicit def numericZResult[A: Numeric]: Numeric[Result[A]] = new Numeric[Result[A]] {
    def add(x: Result[A], y: Result[A]): Result[A] = {
      x.zip(y).
          flatMap{
            case (a,b) =>  {
              val result = a + b
              ZPure.update((n: Int) => n + 1) *>
              ZPure.succeed(result).
              log(s"Add $a and $b $result")}
        }
      // x.zip(y).flatMap{case (a,b) => ZPure.succeed(a + b).log(s"Add $a and $b")}
    }

    def mul(x: Result[A], y: Result[A]): Result[A] = {
      x.zip(y).flatMap{
          case (a,b) =>  {
            val result = a * b
            ZPure.update((n: Int) => n + 1) *>
            ZPure.succeed(result).
            log(s"Multiply $a and $b $result")}
        }
    }

    def sub(x: Result[A], y: Result[A]): Result[A] = {
      x.zip(y).flatMap{
          case (a,b) =>  {
            val result = a - b
            ZPure.update((n: Int) => n + 1) *>
            ZPure.succeed(result).
            log(s"Subtract $b from $a")}
        }
    }

    def div(x: Result[A], y: Result[A]): Result[A] = {
      x.zip(y).flatMap{
          case (a,b) =>  {
            val result = a / b
            ZPure.update((n: Int) => n + 1) *>
            ZPure.succeed(result).
            log(s"Divide $a by $b $result")}
        }
    }
  }

  // Evaluator
  def eval[A: Numeric](exp: Exp[A]): Result[A] =
    exp match {
      case Var(id)    => handleVar(id)
      case Val(value) => 
        ZPure.update((n: Int) => n + 1) *>
        ZPure.succeed(value).flatMap {
          a => 
            ZPure.succeed(a).
            log(s"Literal value $value")
        }
      case Add(l, r)  => handleAdd(l, r)
      case Sub(l, r)  => handleSub(l, r)
      case Mul(l, r)  => handleMul(l, r)
      case Div(l, r)  => handleDiv(l, r)
    }

  def handleVar[A: Numeric](s: String): Result[A] = {
      ZPure.environment[Int, Env[A]].flatMap {
        env =>
          ZPure.update((n: Int) => n + 1) *>
          ZPure.fromOption(env.get(s)).
          mapError(_ => SymbolNotFound).
          flatMap{
            a => ZPure.succeed(a).log(s"Var $s value $a")
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

    val eval1 = eval(exp1).
      provideState(0).
      provide(env1).
      runAll()

    eval1._2 match {
      case Right((opCount,result)) =>
        println(s"Computation succeeded with $result after $opCount operations.")
      case Left(err) => 
        println(s"Computation failed with $err.")
    }

    eval1._1.foreach {
      l => println(l)
    }

  }
}
