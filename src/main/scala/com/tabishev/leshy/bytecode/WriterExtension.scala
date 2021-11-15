package com.tabishev.leshy.bytecode

import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

extension (writer: MethodVisitor) {
  def statement[T](value: T)(using pushable: BytecodeExpression[T]): Unit = {
    push(value)
    pushable.kind(value).popInst.foreach(writer.visitInsn(_))
  }

  def ret[T](value: T)(using pushable: BytecodeExpression[T]): Unit = {
    push(value)
    writer.visitInsn(pushable.kind(value).retInst)
  }

  def putField[T](field: Field, value: T)(using pushable: BytecodeExpression[T]): Unit =
    if (field.isStatic) {
      ???
    } else {
      writer.push(ThisInstance())
      writer.push(value)
      writer.visitFieldInsn(Opcodes.PUTFIELD, field.owner.getInternalName, field.name, field.tpe.getDescriptor)
    }

  def push[T](value: T)(using pushable: BytecodeExpression[T]): Unit = pushable.push(writer, value)
}
