package com.tabishev.leshy.interpreter

object InterpreterUtils {
  def arraysLess(b1: Array[Byte], b2: Array[Byte], orEqual: Boolean): Boolean = {
    var i = 0
    while (i < b1.length) {
      if (b1(i) < b2(i)) return true
      if (b1(i) > b2(i)) return false
      i += 1
    }
    orEqual
  }
}
