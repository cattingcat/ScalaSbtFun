lazy val settings = Seq(
  scalacOptions ++=  Seq(
    "-unchecked",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-deprecation",
    "-encoding",
    "utf8"
  ),
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)




lazy val core = (project in file("core"))
  .settings(settings)

lazy val tgBot = (project in file("tg-bot"))
  .settings(settings)
  .dependsOn(core)

lazy val webApp = (project in file("web-app"))
  .settings(settings)
  .dependsOn(core)


lazy val root = (project in file("."))
  .settings(
    settings,
    name := "todo-app",
    version := "0.23",
    scalaVersion := "2.12.7",
    //mainClass in Compile := Some(mainPath)
  )
  .aggregate(core, tgBot, webApp)
  .dependsOn(core)

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.18"