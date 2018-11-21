package org.zxc123zxc.todoApp.utils

import scala.language.experimental.macros


object Utils {
  def greeting: String = macro MacroUtils.greetingMacro
}
