import scala.math.{Numeric => _}

import cats.implicits._
import cats.data._
import cats.kernel.Monoid
import cats.Applicative

object EvalCats extends App {

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

  // Probably don't need a program effect type, but it won't look like this anyway
  //type EvalResult[A] = ReaderT[[A1] =>> Either[EvalError, A1], Env[A],A]

  // Here's an ADT (abstract data type) for our expression evaluator
  sealed trait Exp[A]
  case class Val[A](value: A) extends Exp[A]
  case class Add[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Sub[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Mul[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Div[A](left: Exp[A], right: Exp[A]) extends Exp[A]
  case class Var[A](identifier: String) extends Exp[A]

  // ZPure[W, S1, S2, R, E, A]

  // to learn: if it has no state or no log, do you use any or nothing?

  // type Result[A] = ZPure[Log, Any, Any, Env[A], Error, A]

//  type Result[A] = WriterT[ReaderT[

  type ResultR[A] = Kleisli[Either[Error,?],Env[A],A]
  type ResultRW[A] = WriterT[ResultR,List[String],A]

  import Numeric.ops._

  implicit def numericZResult[A: Numeric]: Numeric[ResultRW[A]] = new Numeric[ResultRW[A]] {
    def add(x: ResultRW[A], y: ResultRW[A]): ResultRW[A] = {
      //val ass = WriterT.liftF[ResultR,List[String],A](Kleisli.liftF((a: A) => (b: A) => a + b))

      // Reader ap works...
      val kr: ResultR[Int] = Kleisli.liftF(Either.right(10))
      val krInc: ResultR[Int => Int] = Kleisli.liftF(Either.right((a: Int) => a + 1))

      val result = krInc.ap(kr)

      val w: WriterT[Either[String,?],List[String],Int] = WriterT.liftF(Either.right(10))
      val wInc: WriterT[Either[String,?],List[String],Int => Int] = WriterT.liftF(Either.right((a: Int) => a + 1))

      val result2 = w.ap(wInc)

      val m = implicitly[Monoid[List[String]]]
      val app = implicitly[Applicative[Kleisli[Either[Error,?],Env[Int],?]]]

      // (implicit monoidL: Monoid[L], F: Applicative[F])

      val r1: ResultR[Int] = Kleisli.liftF(Either.right(10))
      val wr = WriterT.liftF[Kleisli[Either[Error,?],Env[Int],?],List[String],Int](r1)//(m,app)
      // : WriterT[Kleisli[Either[Error,?],Env[Int],?],List[String],Int]

      // x.ap(y) {

      // }
      ???
      // x.zip(y).flatMap{
      //   case (a,b) => 
      //     ZPure.succeed(a + b).log(s"Add $a and $b")
      // }
    }

    def mul(x: ResultRW[A], y: ResultRW[A]): ResultRW[A] = {
      ??? // x.zip(y).map{case (a,b) => a * b}

    }

    def sub(x: ResultRW[A], y: ResultRW[A]): ResultRW[A] = {
      ??? // x.zip(y).map{case (a,b) => a - b}
    }

    def div(x: ResultRW[A], y: ResultRW[A]): ResultRW[A] = {
      ??? // x.zip(y).map{case (a,b) => a / b}

    }
  }

  // // Evaluator
  // def eval[A: Numeric](exp: Exp[A]): Result[A] =
  //   exp match {
  //     case Var(id)    => handleVar(id)
  //     case Val(value) => ZPure.succeed(value)
  //     case Add(l, r)  => handleAdd(l, r)
  //     case Sub(l, r)  => handleSub(l, r)
  //     case Mul(l, r)  => handleMul(l, r)
  //     case Div(l, r)  => handleDiv(l, r)
  //   }

  // def handleVar[A: Numeric](s: String): Result[A] =
  //   (for (
  //     env <- ZPure.environment[Any, Env[A]];
  //     value <- ZPure
  //       .fromOption(env.get(s))
  //       .mapError(_ => EvalZPure.SymbolNotFound)
  //   ) yield value).log(s"Get $s")

  // def handleAdd[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) + eval(r)
  // def handleMul[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) * eval(r)
  // def handleDiv[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) / eval(r)
  // def handleSub[A: Numeric](l: Exp[A], r: Exp[A]) = eval(l) - eval(r)

    val env1: Env[Int] = Map("x" -> 1, "y" -> 10, "z" -> 100)
    val exp1 = Add(Mul(Val(10), Var("y")),Var("z"))

    println("Hello world!")
}
