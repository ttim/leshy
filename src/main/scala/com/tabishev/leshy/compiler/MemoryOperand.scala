package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{MemoryRef, Runtime}

enum MemoryOperand {
  case Stack(offset: Int)
  case Native(stackOffset: Int)

  def materialize(runtime: Runtime): MemoryRef = this match {
    case MemoryOperand.Stack(offset) => runtime.stack.getRefUnsafe(offset)
    case MemoryOperand.Native(offset) => ???
  }

  def markConst(runtime: Runtime, length: Int, isConst: Boolean) = this match {
    case MemoryOperand.Stack(offset) =>
      runtime.consts.markConst(offset, length, isConst)
    case MemoryOperand.Native(offset) =>
    // do nothing
  }
}
