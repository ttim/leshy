package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, Runtime}

enum MemoryOperand {
  case Stack(offset: FrameOffset)
  case Native(stackOffset: Int)

  def materialize(runtime: Runtime): MemoryRef = this match {
    case MemoryOperand.Stack(offset) => runtime.stack.getRef(offset)
    case MemoryOperand.Native(offset) => ???
  }

  def markConst(runtime: Runtime, bytes: Array[Byte]) = this match {
    case MemoryOperand.Stack(offset) =>
      runtime.consts.markConsts(offset, bytes)
    case MemoryOperand.Native(offset) =>
      // do nothing
  }

  def unmarkConst(runtime: Runtime, length: Int) = this match {
    case MemoryOperand.Stack(offset) =>
      runtime.consts.unmarkConsts(offset, length)
    case MemoryOperand.Native(offset) =>
    // do nothing
  }
}
