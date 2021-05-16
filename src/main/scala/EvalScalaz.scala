import scala.math.{Numeric => _}
import scalaz._
import std.AllFunctions._
import std.AllInstances._

object EvalScalaz extends App {

  // Our Numeric
  object Numeric {
    // This can be used to summon a numeric (same as implicitly)
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

  import Numeric.ops._

  implicit def numericZResult[A: Numeric]: Numeric[WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A]] =
    new Numeric[WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A]] {
        implicit val rwApply = Apply[WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],?]]

    def add(x: WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A], y: WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A]): WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A] = {
      rwApply.apply2(x,y) {
        case (a,b) => a + b
      }
    }

    def mul(x: WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A], y: WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A]): WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A] = {
      rwApply.apply2(x,y) {
        case (a,b) => a * b
      }
    }

    def sub(x: WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A], y: WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A]): WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A] = {
      rwApply.apply2(x,y) {
        case (a,b) => a - b
      }    
    }

    def div(x: WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A], y: WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A]): WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A] = {
      rwApply.apply2(x,y) {
        case (a,b) => a / b
      }   
    }
  }

  // Evaluator
  def eval[A: Numeric](exp: Exp[A]): WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A] =
    exp match {
      case Var(id)    => handleVar(id)
      case Val(value) => WriterT(Kleisli((env: Env[A]) => 
        Right((List(s"Pure value $value"), value)): Either[Error,(List[String],A)]))
      case Add(l, r)  => handleAdd(l, r)
      case Sub(l, r)  => handleSub(l, r)
      case Mul(l, r)  => handleMul(l, r)
      case Div(l, r)  => handleDiv(l, r)
    }

  def handleVar[A: Numeric](s: String): WriterT[List[String],Kleisli[Either[Error,?],Env[A],?],A] = {
    WriterT(Kleisli((env: Env[A]) =>
      env.get(s) match {
        case Some(value) =>
          Right((List(s"Looked up $s and got $value"), value)): Either[Error,(List[String],A)]
        case None => 
          Left(SymbolNotFound): Either[Error,(List[String],A)]
      }))   
  }

  def handleAdd[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) + eval(r)
  def handleMul[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) * eval(r)
  def handleDiv[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) / eval(r)
  def handleSub[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) - eval(r)

  val env1: Env[Int] = Map("x" -> 1, "y" -> 10, "z" -> 100)
  val exp1 = Add(Mul(Val(10), Var("y")),Var("z"))

  println(s"exp1 ${eval(exp1).run(env1)}")
}
