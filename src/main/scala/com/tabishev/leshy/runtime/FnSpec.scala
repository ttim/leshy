package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes

case class FnSpec[T, V](fn: String, input: (T, StackMemory) => Unit, output: Bytes => V) {
  def map[V2](f: V => V2): FnSpec[T, V2] = FnSpec(fn, input, output.andThen(f))

  def contramap[T2](f: T2 => T): FnSpec[T2, V] = FnSpec(fn, {
    (arg, memory) => input(f(arg), memory)
  }, output)
}
