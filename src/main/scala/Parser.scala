import zio.prelude._
import zio.prelude.fx.ZPure
import scala.math.{Numeric => _}

object Parser {

  // final def unit: ZPure[W, S1, S2, R, E, Unit] = as(())

  // Parsing with ZPure
  // State is lookat position, Reader is input string 
  val parseLetter: ZPure[Any,Int,Int,String,String,Char] = {
      ZPure.environment[Int,String].flatMap {
        input =>
          ZPure.get[Int].flatMap {
              pos =>
                val found = input(pos)
                if(found.isLetter) {
                  println("found $found")
                  ZPure.succeed(found).asState(pos + 1)
                } else {
                  ZPure.fail(s"Expecting letter, found $found")
                }
          }
      }
  }

  val parseEnd: ZPure[Any,Int,Int,String,String,Unit] = {
      ZPure.environment[Int,String].flatMap {
        input =>
          ZPure.get[Int].flatMap {
              pos =>
                if(pos == input.size) {
                  ZPure.succeed(()).asState(pos + 1)
                } else {
                  ZPure.fail(s"Expecting end of input")
                }
          }
      }
  }

  // Returns true if the input is all letters
  // Compose the parser for letter or end of input 
  // val allLetters: ZPure[Any,Int,Int,String,String,Unit] = {
  //    parseLetter.orElse {
  //      a =>
  //       ???
  //    }
  // }


  def main(args: Array[String]): Unit = {
    if(args.size > 0) {
      val input = args(0)
      

    } else {
      println("No input")
    }
      
  }
}
