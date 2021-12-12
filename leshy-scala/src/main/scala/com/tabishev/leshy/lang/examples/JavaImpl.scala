package com.tabishev.leshy.lang.examples

object JavaImpl {
  def fib4(n: Int): Int = if (n == 0) 0 else if (n == 1) 1 else fib4(n - 1) + fib4(n - 2)
  def fib8(n: Int): Long = if (n == 0) 0 else if (n == 1) 1 else fib8(n - 1) + fib8(n - 2)

  def ffactorial4(n: Int): Int = {
    var i = n
    var ans = 1
    while (i > 0) {
      ans *= i
      i -= 2
    }
    ans
  }

  def ffactorial8(n: Int): Long = {
    var i: Long = n
    var ans: Long = 1
    while (i > 0) {
      ans *= i
      i -= 2
    }
    ans
  }

  def main(args: Array[String]): Unit =
    println(s"ffact4: ${ffactorial4(10001)}, ffact8: ${ffactorial8(10001)}")
}
