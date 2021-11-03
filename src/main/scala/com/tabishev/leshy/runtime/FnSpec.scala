package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes

case class FnSpec[T, V](fn: String, input: T => Input, output: Bytes => V) {
  def map[V2](f: V => V2): FnSpec[T, V2] = FnSpec(fn, input, output.andThen(f))

  def contramap[T2](f: T2 => T): FnSpec[T2, V] = FnSpec(fn, f.andThen(input), output)
}

object Input {
  val Empty: Input = Input(Bytes.Empty, Consts.Empty)

  def add(bytes: Bytes, isConst: Boolean): Input = Input.Empty.add(bytes, isConst)
}

case class Input private (bytes: Bytes, consts: Consts) {
  def add(toAdd: Bytes, isConst: Boolean): Input = {
    val newConsts = if (isConst)
      consts.markConsts(FrameOffset.nonNegative(bytes.length()), toAdd.get())
    else
      consts
    Input(Bytes.fromBytes(bytes.get() ++ toAdd.get()), newConsts)
  }
}
