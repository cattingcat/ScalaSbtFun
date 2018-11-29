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
    "utf8",
    "-Ypartial-unification"
  ),
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)


lazy val core = (project in file("core"))
  .settings(settings)


val botMainPath = "org.zxc123zxc.todoApp.tgBot.Main"
lazy val tgBot = (project in file("tg-bot"))
  .settings(
    settings,
    mainClass in Compile := Some(botMainPath),
    mainClass in assembly := Some(botMainPath))
  .dependsOn(core)

val webAppPath = "org.zxc123zxc.todoApp.webApp.Main"
lazy val webApp = (project in file("web-app"))
  .settings(
    settings,
    mainClass in Compile := Some(webAppPath),
    mainClass in assembly := Some(webAppPath))
  .dependsOn(core)

val streamsFunPath = "org.zxc123zxc.todoApp.streamsFun.Main"
lazy val streamsFun = (project in file("streams-fun"))
  .settings(
    settings,
    mainClass in Compile := Some(streamsFunPath),
    mainClass in assembly := Some(streamsFunPath))
  .dependsOn(core)

lazy val root = (project in file("."))
  .settings(
    settings,
    name := "todo-app",
    version := "0.23",
    scalaVersion := "2.12.7",
    //mainClass in Compile := Some(mainPath)
  )
  .aggregate(core, tgBot, webApp, streamsFun)
  .dependsOn(core)
