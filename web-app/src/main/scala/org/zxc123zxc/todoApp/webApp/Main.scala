package org.zxc123zxc.todoApp.webApp

import org.zxc123zxc.todoApp.core.WorkItem


object Main {
  def main(args: Array[String]): Unit = {
    val item = new WorkItem("web App")

    println(item.title)
  }
}
