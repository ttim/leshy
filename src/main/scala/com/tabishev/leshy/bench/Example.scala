package com.tabishev.leshy.bench

object Example {
  def fact4(n: Int): Int = {
    var i = n
    var ans = 1
    while (i > 0) {
      ans *= i
      i -= 1
    }
    ans
  }

  def fact8(n: Int): Long = {
    var i: Long = n
    var ans: Long = 1
    while (i > 0) {
      ans *= i
      i -= 1
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    System.out.println(fact4(33))
    System.out.println(fact8(33))
  }
}
