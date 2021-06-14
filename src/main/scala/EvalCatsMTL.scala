import scala.math.{Numeric => _}

import cats.implicits._
import cats.mtl.implicits._


import cats.data._
import cats.Id
import cats.kernel.Monoid
import cats.{Applicative,Monad}
import cats.mtl.Ask
import cats.mtl.Raise
import cats.mtl.Tell
import cats.mtl.Stateful
import cats.MonadError
import cats.data
import java.lang
import scala.collection.immutable

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

  implicit def numericZResult[A: Numeric]: Numeric[WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A]] = 
    new Numeric[WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A]] {

      override def add(a: WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A], 
        b: WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A]) 
        = ???
      override def sub(a: WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A], 
        b: WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A]) 
        = ???   
      override def mul(a: WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A], 
        b: WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A]) 
        = ???   
      override def div(a: WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A], 
        b: WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],A]) 
        = ???

  }

  def handleVar2[A: Numeric](s: String): WriterT[Kleisli[Either[Error,?],Env[A],?],List[String],A] = {
      WriterT(Kleisli((env: Env[A]) =>
      env.get(s) match {
        case Some(value) =>
          Right((List(s"Looked up $s and got $value"), value)): Either[Error,(List[String],A)]
        case None => 
          Left(SymbolNotFound): Either[Error,(List[String],A)]
      }))   
  }

  def handleAdd[F[_], A: Numeric](l: Exp[A], r: Exp[A])(implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: MonadError[F, Error]): F[A] = {
    E.map2(eval2(l),eval2(r))(_ + _)
  }

  def handleMul[F[_], A: Numeric](l: Exp[A], r: Exp[A])(implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: MonadError[F, Error]): F[A] = {
    E.map2(eval2(l),eval2(r))(_ * _)
  }

  def handleDiv[F[_], A: Numeric](l: Exp[A], r: Exp[A])(implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: MonadError[F, Error]): F[A] = {
    E.map2(eval2(l),eval2(r))(_ / _)
  }

  def handleSub[F[_], A: Numeric](l: Exp[A], r: Exp[A])(implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: MonadError[F, Error]): F[A] = {
    E.map2(eval2(l),eval2(r))(_ - _)
  }

  val env1: Env[Int] = Map("x" -> 1, "y" -> 10, "z" -> 100)

  val exp1 = Add(
              Add(
                Add(
                  Add(
                    Val(20),
                    Var("y")
                  ),
                  Var("x")),
                Var("y")
              ),
              Var("z"))

  def eval2[F[_],A: Numeric](exp: Exp[A])(implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: MonadError[F, Error]): F[A] = 
      exp match {
        case Val(value) => E.pure(value)
        case Var(id) => handleVar(id)
        case Add(left,right) => handleAdd(left,right)
      }

  def handleVar[F[_],A: Numeric](id: String)(implicit L: Tell[F,List[String]], R: Ask[F, Env[A]], E: MonadError[F, Error]): F[A] = 
    R.ask.flatMap {
      env => 
        env.get(id) match {
          case Some(value) => L.tell(List(s"Var $id was $value")) *> E.pure(value)
          case None => E.raiseError(SymbolNotFound)
        }
    }

  val almost2 =
    eval2[WriterT[EitherT[ReaderT[Id, Env[Int], ?], EvalCatsMTL.Error, ?],List[String],?],Int](exp1)

  println(almost2.value.value.run(env1))
}
