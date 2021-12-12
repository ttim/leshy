package com.tabishev.leshy.bytecode

import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}

sealed trait BranchModifier {
  import BranchModifier._

  // int1 :op: int2
  def intOpcode: Int = this match {
    case GT => Opcodes.IF_ICMPGT
    case LE => Opcodes.IF_ICMPLE
    case EQ => Opcodes.IF_ICMPEQ
  }

  // int :op: 0
  def cmpOpcode: Int = this match {
    case GT => Opcodes.IFGT
    case LE => Opcodes.IFLE
    case EQ => Opcodes.IFEQ
  }
}

object BranchModifier {
  case object GT extends BranchModifier
  case object LE extends BranchModifier
  case object EQ extends BranchModifier
}

object WriterExtension {
  implicit class Extension(writer: MethodVisitor) {
    def statement(value: Expression): Unit = {
      val kind = push(value)
      kind.popInst.foreach(writer.visitInsn(_))
    }

    def ret(value: Expression): Unit = {
      val kind = push(value)
      writer.visitInsn(kind.retInst)
    }

    def branch(arg1: Expression, modifier: BranchModifier, arg2: Expression, label: Label): Unit = {
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

    def branch(booleanArg: Expression, label: Label): Unit = {
      writer.push(booleanArg)
      writer.visitJumpInsn(Opcodes.IFGT, label)
    }

    def branch(label: Label): Unit = writer.visitJumpInsn(Opcodes.GOTO, label)

    def putField(field: Field, value: Expression): Unit =
      if (field.isStatic) {
        ???
      } else {
        writer.push(ThisInstance())
        writer.push(value)
        writer.visitFieldInsn(Opcodes.PUTFIELD, field.owner.getInternalName, field.name, field.tpe.getDescriptor)
      }

    def storeVar(idx: Int, value: Expression): Unit = {
      val kind = writer.push(value)
      writer.visitVarInsn(kind.storeInst.get, idx)
    }

    def push(value: Expression): BytecodeExpressionKind = value.push(writer)
  }
}
