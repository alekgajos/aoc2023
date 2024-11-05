val scala3Version = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "day20",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-Wunused:locals",
      "-Wunused:params",
      "-Wunused:imports",
      "-Wunused:linted"
    ),
    libraryDependencies += "com.lihaoyi" % "ammonite" % "3.0.0" cross CrossVersion.full,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.9.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(
    file,
    """object amm extends App { ammonite.AmmoniteMain.main(args) }"""
  )
  Seq(file)
}.taskValue
