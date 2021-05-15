ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.justinhj"
ThisBuild / organizationName := "example"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)

scalacOptions ++= Seq(
	"-Vimplicit-conversions"
	)

lazy val root = (project in file("."))
  .settings(
    name := "ZPurePlay",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "1.0.5",
      "dev.zio" %% "zio-test" % "1.0.5" % Test,
      "dev.zio" %% "zio-prelude" % "1.0.0-RC3",
      "org.scalaz" %% "scalaz-core" % "7.3.3",
      "org.typelevel" %% "cats-core" % "2.3.0"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
