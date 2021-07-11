ThisBuild / scalaVersion     := "3.0.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.justinhj"
ThisBuild / organizationName := "example"

/* addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full) */

scalacOptions ++= Seq(
//	"-Vimplicit-conversions"
	)

val zioVersion = "1.0.9"

lazy val root = (project in file("."))
  .settings(
    name := "ZPurePlay",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test",
      "dev.zio" %% "zio-test-magnolia" % zioVersion % "test",
      "dev.zio" %% "zio-prelude" % "1.0.0-RC5",
      /* "org.scalaz" %% "scalaz-core" % "7.3.3", */
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.typelevel" %% "cats-mtl" % "1.2.1",
      "com.lihaoyi" %% "utest" % "0.7.10" % "test"
    ),
    testFrameworks ++= Seq(
      new TestFramework("zio.test.sbt.ZTestFramework")
    )
  )
