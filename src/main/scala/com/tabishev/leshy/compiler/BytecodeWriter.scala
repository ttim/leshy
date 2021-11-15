package com.tabishev.leshy.compiler

import com.tabishev.leshy.bytecode._
import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import scala.reflect.ClassTag

object MemoryOps {
  val Runtime: BytecodeExpression = Param.idx[Runtime](0)
  val Stack: BytecodeExpression = InvokeMethod.virtual(classOf[Runtime], "stack", Runtime)

  def frameOffset(offset: FrameOffset): BytecodeExpression =
    InvokeMethod.static(classOf[FrameOffset], "nonNegative", Ops.int(offset.get))

  def memoryOperand(operand: MemoryOperand): BytecodeExpression = operand match {
    case MemoryOperand.Stack(offset) =>
      InvokeMethod.virtual(classOf[StackMemory], "getRef", Stack, frameOffset(offset))
    case MemoryOperand.Native(offset) =>
      ???
  }

  def getInt(op: MemoryOperand): BytecodeExpression =
    InvokeMethod.virtual(classOf[MemoryRef], "getInt", memoryOperand(op))
  def putInt(op: MemoryOperand, value: BytecodeExpression): BytecodeExpression =
    InvokeMethod.virtual(classOf[MemoryRef], "putInt", memoryOperand(op), value)
  def getLong(op: MemoryOperand): BytecodeExpression =
    InvokeMethod.virtual(classOf[MemoryRef], "getLong", memoryOperand(op))
  def putLong(op: MemoryOperand, value: BytecodeExpression): BytecodeExpression =
    InvokeMethod.virtual(classOf[MemoryRef], "putLong", memoryOperand(op), value)
}
