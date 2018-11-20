package org.zxc123zxc.todoApp

import org.zxc123zxc.todoApp.core.WorkItem


object Main {
  def main(args: Array[String]): Unit = {
    val item = new WorkItem("root app")
    println(item.title)
  }
}
