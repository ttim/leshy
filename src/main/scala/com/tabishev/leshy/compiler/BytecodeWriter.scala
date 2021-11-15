package com.tabishev.leshy.compiler

import com.tabishev.leshy.bytecode._
import com.tabishev.leshy.bytecode.intBytecodeExpression
import com.tabishev.leshy.bytecode.paramBytecodeExpression
import com.tabishev.leshy.bytecode.valueBytecodeExpression
import com.tabishev.leshy.bytecode.invokeMethodBytecodeExpression
import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import scala.reflect.ClassTag

given memoryOperandPushable: BytecodeExpression[MemoryOperand] = BytecodeExpression.from {
  case MemoryOperand.Stack(offset) =>
    InvokeMethod.virtual(classOf[StackMemory], "getRef", ContextStack(), offset)
  case MemoryOperand.Native(offset) =>
    ???
}

case class ContextStack()
case class ContextRuntime()

given runtimePushable: BytecodeExpression[ContextRuntime] =
  BytecodeExpression.from(Param.idx[Runtime](0))
given frameOffsetPushable: BytecodeExpression[FrameOffset] =
  BytecodeExpression.from(value => InvokeMethod.static(classOf[FrameOffset], "nonNegative", value.get))
given stackPushable: BytecodeExpression[ContextStack] =
  BytecodeExpression.from(InvokeMethod.virtual(classOf[Runtime], "stack", ContextRuntime()))

object MemoryOps {
  def getInt(op: MemoryOperand): GetInt = GetInt(op)
  def putInt[T: BytecodeExpression](op: MemoryOperand, value: T): PutInt = PutInt(op, Value(value))
  def getLong(op: MemoryOperand): GetLong = GetLong(op)
  def putLong[T: BytecodeExpression](op: MemoryOperand, value: T): PutLong = PutLong(op, Value(value))
}

case class GetInt(op: MemoryOperand)
case class PutInt(op: MemoryOperand, value: Value[_])
case class GetLong(op: MemoryOperand)
case class PutLong(op: MemoryOperand, value: Value[_])

given getIntExpr: BytecodeExpression[GetInt] = BytecodeExpression.from { value =>
  InvokeMethod.virtual(classOf[MemoryRef], "getInt", value.op)
}

given putIntExpr: BytecodeExpression[PutInt] = BytecodeExpression.from { value =>
  InvokeMethod.virtual(classOf[MemoryRef], "putInt", value.op, value.value)
}

given getLongExpr: BytecodeExpression[GetLong] = BytecodeExpression.from { value =>
  InvokeMethod.virtual(classOf[MemoryRef], "getLong", value.op)
}

given putLongExpr: BytecodeExpression[PutLong] = BytecodeExpression.from { value =>
  InvokeMethod.virtual(classOf[MemoryRef], "putLong", value.op, value.value)
}