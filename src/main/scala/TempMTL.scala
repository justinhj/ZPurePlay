import cats._
import cats.implicits._
import cats.mtl._
import cats.data.{WriterT,EitherT,ReaderT}
import cats.mtl.implicits._
object TempMTL extends App {
  // Reverses all strings except for "irreversible"
  def raiseExample[F[_]](s: String)(implicit E: Raise[F, String], A: Applicative[F]): F[String] = {
    if(s == "irreversible") {
      E.raise("Cannot be reversed")
    } else {
      A.pure(s.reverse)
    }
  }

  println(raiseExample[[A1] =>> Either[String,A1]]("irreversible"))
  println(raiseExample[[A1] =>> Either[String,A1]]("Hello world"))

  // As above but add logging
  def raiseWithTellExample[F[_]](s: String)
    (implicit T: Tell[F, List[String]], E: Raise[F, String], A: Applicative[F]): F[String] = {
    if(s == "irreversible") {
      E.raise("Cannot be reversed")
    } else {
      T.tell(List(s"Reversed string $s")) *>
        A.pure(s.reverse)
    }
  }

  // Let's add Tell (WriterT)

  // Wrapping Either with WriterT does not work, suggests EitherT, WriterT, Writer instead
  //println(raiseWithTellExample[WriterT[Either[String,_],List[String],_]]("Hello world"))

  // Does work if you put EitherT on the outside and use Id as the inner monad
  println(raiseWithTellExample[[A1] =>> EitherT[[A2] =>> WriterT[Id,List[String],A2],String,A1]]("Hello world"))
  println(raiseWithTellExample[[A1] =>> EitherT[[A2] =>> WriterT[Id,List[String],A2],String,A1]]("irreversible"))

  // How about same thing with WriterT on the outside?
  // Nope. Recommends EitherT[WriterT[EitherT[[A]]]] which wouldn't work since A has the wrong shape
  //println(raiseWithTellExample[WriterT[EitherT[Id,String,_],List[String],_]]("Hello world"))

  // Let's add Ask (ReaderT) to give a list of irreversible strings
  def raiseWithTellAndAskExample[F[_]](s: String)
    (implicit R: Ask[F,List[String]], T: Tell[F, List[String]], E: Raise[F, String], M: Monad[F]): F[String] = {
      R.ask.flatMap {
        irreversibles =>
          if(irreversibles.contains(s)) {
            E.raise(s"$s cannot be reversed")
          } else {
            T.tell(List(s"Reversed string $s")) *>
              M.pure(s.reverse)
          }
      }
  }

  println(raiseWithTellAndAskExample[EitherT[ReaderT[WriterT[Id,List[String],_],List[String],_],String,_]]("Hello world")
    .value.run(List("irreversible","Hello world")))


}
