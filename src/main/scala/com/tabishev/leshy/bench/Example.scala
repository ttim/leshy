package com.tabishev.leshy.bench

object Example {
  def ffact4(n: Int): Int = {
    var i = n
    var ans = 1
    while (i > 0) {
      ans *= i
      i -= 2
    }
    ans
  }

  def ffact8(n: Int): Long = {
    var i: Long = n
    var ans: Long = 1
    while (i > 0) {
      ans *= i
      i -= 2
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    System.out.println(ffact4(777777))
    System.out.println(ffact8(777777))
  }
}
