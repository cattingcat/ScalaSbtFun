package org.zxc123zxc.todoApp.utils

import java.util.Date
import scala.language.experimental.macros


import scala.reflect.macros.blackbox
//import scala.reflect.macros.whitebox.Context

// https://docs.scala-lang.org/overviews/macros/blackbox-whitebox.html
// https://docs.scala-lang.org/overviews/macros/bundles.html
// https://docs.scala-lang.org/overviews/macros/overview.html
// https://docs.scala-lang.org/overviews/macros/typemacros.html
// https://habr.com/company/enterra/blog/224229/
// scalacOptions += "-Ymacro-debug-lite"   for debugging



class MacroUtils(val c: blackbox.Context) {
  import c.universe._

  def greetingMacro: c.Tree = {
    //val now = new Date().toString
    val qq = q"(2 + 2).toString"
//    val const = Literal(Constant(s"Hi from macro  $now   \n $qq @@"))
//    const
    qq
  }
}