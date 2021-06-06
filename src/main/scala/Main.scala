import zio.console.putStrLn
import zio.prelude.fx.ZPure
import zio.{App, ExitCode, URIO}

object Main extends App {

  type WString = List[String]

  sealed trait Error

  object OddNumberProvided extends Error

  object EvenNumberProvided extends Error

  def incrementEven(a: Int): ZPure[WString, Any, Any, Any, Error, Int] = {
    if (a % 2 == 1) ZPure.fail(OddNumberProvided)
    else ZPure.succeed(a + 1)
  }

  def doubleOdd(a: Int): ZPure[WString, Any, Any, Any, Error, Int] = {
    if (a % 2 == 0) ZPure.fail(EvenNumberProvided)
    else ZPure.succeed(a * 2)
  }

  val s1: ZPure[WString, Any, Any, Any, Error, (Int, Int, Int)] = for (
    a <- ZPure.succeed(4);
    _ <- ZPure.log(List("init 4"));
    b <- incrementEven(a);
    _ <- ZPure.log(List(s"increment by $a new value is $b"));
    c <- doubleOdd(b);
    _ <- ZPure.log(List(s"double $b new value is $c"))
  ) yield (a, b, c)


  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    val s1r = s1.runEither.getOrElse((8, 8, 8))
    val s1r2 = s1.runAll(())

    putStrLn(s"Welcome to your first ZIO Prelude app! ${s1r2}").exitCode
  }
}
