package org.zxc123zxc.todoApp.tgBot

import akka.actor.{Actor, ActorSystem, Props}
import org.zxc123zxc.todoApp.core.WorkItem

import scala.io.StdIn


class TestActor extends Actor {
  override def receive: Receive = {
    case "sayHi" => println("Hey from actor")
    case _ => println("Unknown command")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val item = new WorkItem("Tg bot")
    println(item.title)

    val actorSystem = ActorSystem("test_actor_system")
    val testActor = actorSystem.actorOf(Props[TestActor], "test_actor")

    testActor ! "qwe"
    println("after qwe")

    Thread.sleep(1000)

    testActor ! "sayHi"
    println("after sayHi")

    StdIn.readLine()
  }
}
