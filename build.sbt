ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.justinhj"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "ZPurePlay",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "1.0.5",
      "dev.zio" %% "zio-test" % "1.0.5" % Test,
      "dev.zio" %% "zio-prelude" % "1.0.0-RC3" 
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
