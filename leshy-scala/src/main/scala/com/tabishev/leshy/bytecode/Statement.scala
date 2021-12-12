package com.tabishev.leshy.bytecode

import org.objectweb.asm.{Label, MethodVisitor, Opcodes}
import com.tabishev.leshy.bytecode.{Expression => BytecodeExpression}

trait Statement {
  def write(writer: MethodVisitor): Unit
}

object Statement {
  implicit class WriterExtension(writer: MethodVisitor) {
    def write(statement: Statement): Unit = statement.write(writer)
  }

  case class Expr(value: BytecodeExpression) extends Statement {
    override def write(writer: MethodVisitor): Unit = {
        val kind = value.push(writer)
        kind.popInst.foreach(writer.visitInsn)
    }
  }

  case class Return(value: BytecodeExpression) extends Statement {
    override def write(writer: MethodVisitor): Unit = {
      val kind = value.push(writer)
      writer.visitInsn(kind.retInst)
    }
  }

  case class Branch(arg1: BytecodeExpression, modifier: BranchModifier, arg2: BytecodeExpression, label: Label) extends Statement {
    override def write(writer: MethodVisitor): Unit = {
      val kind = arg1.push(writer)
      val kind2 = arg2.push(writer)
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
  }

  case class BooleanBranch(booleanArg: BytecodeExpression, label: Label) extends Statement {
    override def write(writer: MethodVisitor): Unit = {
      booleanArg.push(writer)
      writer.visitJumpInsn(Opcodes.IFGT, label)
    }
  }

  case class Goto(label: Label) extends Statement {
    override def write(writer: MethodVisitor): Unit = writer.visitJumpInsn(Opcodes.GOTO, label)
  }

  case class PutField(field: Field, value: BytecodeExpression) extends Statement {
    override def write(writer: MethodVisitor): Unit =
      if (field.isStatic) {
        ???
      } else {
        ThisInstance().push(writer)
        value.push(writer)
        writer.visitFieldInsn(Opcodes.PUTFIELD, field.owner.getInternalName, field.name, field.tpe.getDescriptor)
      }
  }

  case class StoreVar(idx: Int, value: BytecodeExpression) extends Statement {
    override def write(writer: MethodVisitor): Unit = {
      val kind = value.push(writer)
      writer.visitVarInsn(kind.storeInst.get, idx)
    }
  }
}

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

