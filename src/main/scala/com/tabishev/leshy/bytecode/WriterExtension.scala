package com.tabishev.leshy.bytecode

import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}

enum BranchModifier {
  case GT
  case LE

  // int1 :op: int2
  def intOpcode: Int = this match {
    case GT => Opcodes.IF_ICMPGT
    case LE => Opcodes.IF_ICMPLE
  }

  // int :op: 0
  def cmpOpcode: Int = this match {
    case GT => Opcodes.IFGT
    case LE => Opcodes.IFLE
  }
}

extension (writer: MethodVisitor) {
  def statement(value: BytecodeExpression): Unit = {
    val kind = push(value)
    kind.popInst.foreach(writer.visitInsn(_))
  }

  def ret(value: BytecodeExpression): Unit = {
    val kind = push(value)
    writer.visitInsn(kind.retInst)
  }

  def branch(arg1: BytecodeExpression, modifier: BranchModifier, arg2: BytecodeExpression, label: Label): Unit = {
    val kind = push(arg1)
    val kind2 = push(arg2)
    assert(kind == kind2)
    kind match {
      case BytecodeExpressionKind.Int =>
        writer.visitJumpInsn(modifier.intOpcode, label)
      case BytecodeExpressionKind.Long =>
        writer.visitInsn(Opcodes.LCMP)
        writer.visitJumpInsn(modifier.cmpOpcode, label)
      case BytecodeExpressionKind.Void =>
        throw new UnsupportedOperationException
      case BytecodeExpressionKind.Object =>
        throw new UnsupportedOperationException
    }
  }

  def putField(field: Field, value: BytecodeExpression): Unit =
    if (field.isStatic) {
      ???
    } else {
      writer.push(ThisInstance())
      writer.push(value)
      writer.visitFieldInsn(Opcodes.PUTFIELD, field.owner.getInternalName, field.name, field.tpe.getDescriptor)
    }

  def storeVar(idx: Int, value: BytecodeExpression): Unit = {
    val kind = writer.push(value)
    writer.visitVarInsn(kind.storeInst.get, idx)
  }

  def push(value: BytecodeExpression): BytecodeExpressionKind = value.push(writer)
}
