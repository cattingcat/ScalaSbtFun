package org.zxc123zxc.todoApp.tgBot

import org.zxc123zxc.todoApp.core.WorkItem


object Main {
  def main(args: Array[String]): Unit = {
    val item = new WorkItem("qew")

    println(item.title)
  }
}
