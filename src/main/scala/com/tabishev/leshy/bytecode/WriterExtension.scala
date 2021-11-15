package com.tabishev.leshy.bytecode

import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}

extension (writer: MethodVisitor) {
  def statement(value: BytecodeExpression): Unit = {
    val kind = push(value)
    kind.popInst.foreach(writer.visitInsn(_))
  }

  def ret(value: BytecodeExpression): Unit = {
    val kind = push(value)
    writer.visitInsn(kind.retInst)
  }

  def branch(arg1: BytecodeExpression, opcode: Int, arg2: BytecodeExpression, label: Label): Unit = {
    push(arg1)
    push(arg2)
    writer.visitJumpInsn(opcode, label)
  }

  def putField(field: Field, value: BytecodeExpression): Unit =
    if (field.isStatic) {
      ???
    } else {
      writer.push(ThisInstance())
      writer.push(value)
      writer.visitFieldInsn(Opcodes.PUTFIELD, field.owner.getInternalName, field.name, field.tpe.getDescriptor)
    }

  def push(value: BytecodeExpression): BytecodeExpression.Kind = value.push(writer)
}
