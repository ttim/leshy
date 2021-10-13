package com.tabishev.leshy.bench

object Example {
  def fib4(n: Int): Int = if (n <= 1) 1 else fib4(n - 1) + fib4(n - 2)
  def fib8(n: Int): Long = if (n <= 1) 1 else fib8(n - 1) + fib8(n - 2)

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
}
