import scala.math.{Numeric => _}
import cats.implicits._
import cats.data._
import cats.Monad
import cats.mtl.{Ask, Raise, Tell, Stateful}

/**
 * Convert Cats version of Eval to utilize MTL
 */

object EvalCatsMTL extends App {

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
    def isZero(a: T): Boolean
  }

  implicit val numericInt: Numeric[Int] = new Numeric[Int] {
    def add(a: Int, b: Int): Int = a + b
    def sub(a: Int, b: Int): Int = a - b
    def div(a: Int, b: Int): Int = a / b
    def mul(a: Int, b: Int): Int = a * b
    def isZero(a: Int): Boolean = a == 0
  }

  implicit val numericLong: Numeric[Long] = new Numeric[Long] {
    def add(a: Long, b: Long): Long = a + b
    def mul(a: Long, b: Long): Long = a * b
    def sub(a: Long, b: Long): Long = a - b
    def div(a: Long, b: Long): Long = a / b
    def isZero(a: Long): Boolean = a == 0L
  }

  // Error type (for errors that occur evaluating expressions)
  sealed trait Error
  object SymbolNotFound extends Error
  object DivisionByZero extends Error

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

  def handleAdd[F[_], A: Numeric](l: Exp[A], r: Exp[A])
      (implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: Raise[F, Error], S: Stateful[F,Int], M: Monad[F]): F[A] = {
    eval(l).flatMap {
      la =>
        eval(r).flatMap {
          ra =>
            val c = la + ra
            L.tell(List(s"Add $la and $ra gave $c")) *> M.pure(c)
        }
    }
  }
  
  def handleMul[F[_], A: Numeric](l: Exp[A], r: Exp[A])
      (implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: Raise[F, Error], S: Stateful[F,Int], M: Monad[F]): F[A] = {
    eval(l).flatMap {
          la =>
            eval(r).flatMap {
              ra =>
                val c = la * ra
                L.tell(List(s"Multiply $la and $ra gave $c")) *> M.pure(c)
            }
    }  
  }

  def handleDiv[F[_], A: Numeric](l: Exp[A], r: Exp[A])
      (implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: Raise[F, Error], S: Stateful[F,Int], M: Monad[F]): F[A] = {
    eval(l).flatMap {
      la =>
        eval(r).flatMap {
          ra =>
            if(implicitly[Numeric[A]].isZero(ra)) {
              E.raise(DivisionByZero)
            } else {
              val c = la / ra
              L.tell(List(s"Div $la by $ra gave $c")) *> M.pure(c)
            }
        }
    }
  }

  def handleSub[F[_], A: Numeric](l: Exp[A], r: Exp[A])
      (implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: Raise[F, Error], S: Stateful[F,Int], M: Monad[F]): F[A] = {
    eval(l).flatMap {
        la =>
          eval(r).flatMap {
            ra =>
              val c = la - ra
              L.tell(List(s"Subtract $ra from $la gave $c")) *> M.pure(c)
          }
      }
  }

  def handleVar[F[_],A: Numeric](id: String)
    (implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: Raise[F, Error], S: Stateful[F,Int], M: Monad[F]): F[A] =
    R.ask.flatMap {
      env => 
        env.get(id) match {
          case Some(value) => L.tell(List(s"Var $id was $value")) *> M.pure(value)
          case None => E.raise(SymbolNotFound)
        }
    }

  def eval[F[_],A: Numeric](exp: Exp[A])
  (implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: Raise[F, Error], S: Stateful[F,Int], M: Monad[F]): F[A] =
      exp match {
        case Val(value) => S.modify(_ + 1) *> M.pure(value)
        case Var(id) => S.modify(_ + 4) *> handleVar(id)
        case Add(left,right) => S.modify(_ + 2) *> handleAdd(left,right)
        case Sub(left,right) => S.modify(_ + 2) *> handleSub(left,right)
        case Div(left,right) => S.modify(_ + 2) *> handleDiv(left,right)
        case Mul(left,right) => S.modify(_ + 2) *> handleMul(left,right)
      }

  val env1: Env[Int] = Map("x" -> 1, "y" -> 10, "z" -> 100)

  val exp1 = Mul(
    Add(
      Sub(
        Div(
          Val(20),
          Var("y")
        ),
        Var("x")),
      Var("y")
    ),
    Var("z"))

  // "materialize" the program by running it with an expression and defining the types to use
  val program =
    eval[
      EitherT[
        StateT[
          WriterT[
            Reader[Env[Int], *],
            List[String],
            *],
          Int,
          *],
        Error,
        *],Int](exp1)

  val runResult = program.value.run(0).run(env1)

  runResult._2._2 match {
    case Left(err) => println(s"Failed with error $err")
    case Right(result) =>
      println(s"Result: $result State: ${runResult._2._1}")
      runResult._1.foreach {
        entry =>
          println(s"\t$entry")
      }
  }
}