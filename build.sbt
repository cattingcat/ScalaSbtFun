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
