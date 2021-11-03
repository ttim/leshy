package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{Consts, FrameOffset, MemoryRef, Runtime}

enum MemoryOperand {
  case Stack(offset: FrameOffset)
  case Native(stackOffset: Int)

  def materialize(runtime: Runtime): MemoryRef = this match {
    case MemoryOperand.Stack(offset) => runtime.stack.getRef(offset)
    case MemoryOperand.Native(offset) => ???
  }

  def markConst(consts: Consts, bytes: Array[Byte]): Consts = this match {
    case MemoryOperand.Stack(offset) =>
      consts.markConsts(offset, bytes)
    case MemoryOperand.Native(offset) =>
      // do nothing
      consts
  }

  def unmarkConst(consts: Consts, length: Int): Consts = this match {
    case MemoryOperand.Stack(offset) =>
      consts.unmarkConsts(offset, length)
    case MemoryOperand.Native(offset) =>
      // do nothing
      consts
  }
}
