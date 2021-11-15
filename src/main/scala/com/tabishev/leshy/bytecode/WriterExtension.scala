package com.tabishev.leshy.bytecode

import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}

extension (writer: MethodVisitor) {
  def statement[T](value: T)(using pushable: BytecodeExpression[T]): Unit = {
    val kind = push(value)
    kind.popInst.foreach(writer.visitInsn(_))
  }

  def ret[T](value: T)(using pushable: BytecodeExpression[T]): Unit = {
    val kind = push(value)
    writer.visitInsn(kind.retInst)
  }

  def branch[T1, T2](arg1: T1, opcode: Int, arg2: T2, label: Label)(using BytecodeExpression[T1], BytecodeExpression[T2]): Unit = {
    push(arg1)
    push(arg2)
    writer.visitJumpInsn(opcode, label)
  }

  def putField[T](field: Field, value: T)(using pushable: BytecodeExpression[T]): Unit =
    if (field.isStatic) {
      ???
    } else {
      writer.push(ThisInstance())
      writer.push(value)
      writer.visitFieldInsn(Opcodes.PUTFIELD, field.owner.getInternalName, field.name, field.tpe.getDescriptor)
    }

  def push[T](value: T)(using pushable: BytecodeExpression[T]): BytecodeExpression.Kind = pushable.push(writer, value)
}
