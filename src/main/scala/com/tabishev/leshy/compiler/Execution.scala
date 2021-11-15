package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.runtime.{Consts, FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}
import com.tabishev.leshy.bytecode._
import com.tabishev.leshy.bytecode.BytecodeExpression._

sealed abstract class Execution {
  def execute(runtime: Runtime): Unit
  def write(writer: MethodVisitor): Unit = throw new NotImplementedError(toString)

  def markConsts(consts: Consts): Consts
  def stackSize(before: Int): Int = before
}

sealed abstract class NonConstExecution extends Execution {
  val length: Int
  val dst: MemoryOperand

  final def markConsts(consts: Consts): Consts = dst.unmarkConst(consts, length)
}

sealed abstract class NonConstExecution4 extends NonConstExecution {
  final val length: Int = 4
}

sealed abstract class NonConstExecution8 extends NonConstExecution {
  final val length: Int = 8
}

object Const {
  final case class Write4(value: Int, dst: MemoryOperand) extends Execution {
    override def execute(runtime: Runtime): Unit = dst.materialize(runtime).putInt(value)
    override def write(writer: MethodVisitor): Unit = writer.statement(MemoryOps.putInt(dst, const(value)))

    override def markConsts(consts: Consts): Consts = dst.markConst(consts, Bytes.fromInt(value).get())
  }

  final case class Write8(value: Long, dst: MemoryOperand) extends Execution {
    override def execute(runtime: Runtime): Unit = dst.materialize(runtime).putLong(value)
    override def write(writer: MethodVisitor): Unit = writer.statement(MemoryOps.putLong(dst, const(value)))

    override def markConsts(consts: Consts): Consts = dst.markConst(consts, Bytes.fromLong(value).get())
  }
}

object Mark {
  // Specialize can't implemented simalry because execution assumes spec ctx not changing between runs
  final case class NotSpecialize(length: Int, dst: MemoryOperand) extends NonConstExecution {
    override def execute(runtime: Runtime): Unit = ()
    override def write(writer: MethodVisitor): Unit = ()
  }
}

object Stack {
  final case class SetSize(oldSize: Int, newSize: Int) extends Execution {
    override def execute(runtime: Runtime): Unit = runtime.stack.setFramesize(newSize)
    override def write(writer: MethodVisitor): Unit =
      writer.statement(invokeVirtual(classOf[StackMemory], "setFramesize", MemoryOps.Stack, const(newSize)))

    override def markConsts(consts: Consts): Consts =
      if (newSize > oldSize)
        consts.markConsts(FrameOffset.nonNegative(oldSize), Array.fill[Byte](newSize - oldSize)(0))
      else
        consts.unmarkConsts(FrameOffset.nonNegative(newSize), oldSize - newSize)

    override def stackSize(before: Int): Int = {
      assert(before == oldSize)
      newSize
    }
  }
}

object Sum {
  // MM - memory, memory
  final case class MM4(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() + op2.materialize(runtime).getInt())
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putInt(dst, sum(MemoryOps.getInt(op1), MemoryOps.getInt(op2))))
  }

  final case class MC4(op1: MemoryOperand, op2: Int, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() + op2)
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putInt(dst, sum(MemoryOps.getInt(op1), const(op2))))
  }

  final case class MM8(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() + op2.materialize(runtime).getLong())
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putLong(dst, sum(MemoryOps.getLong(op1), MemoryOps.getLong(op2))))
  }

  final case class MC8(op1: MemoryOperand, op2: Long, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() + op2)
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putLong(dst, sum(MemoryOps.getLong(op1), const(op2))))
  }

  def length4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int, dst: MemoryOperand): Execution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM4(op1, op2, dst)
      case (op1: MemoryOperand, op2: Int) => MC4(op1, op2, dst)
      case (op1: Int, op2: MemoryOperand) => MC4(op2, op1, dst)
      case (op1: Int, op2: Int) => Const.Write4(op1 + op2, dst)
    }

  def length8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long, dst: MemoryOperand): Execution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM8(op1, op2, dst)
      case (op1: MemoryOperand, op2: Long) => MC8(op1, op2, dst)
      case (op1: Long, op2: MemoryOperand) => MC8(op2, op1, dst)
      case (op1: Long, op2: Long) => Const.Write8(op1 + op2, dst)
    }
}

object Mult {
  // MM - memory, memory
  final case class MM4(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() * op2.materialize(runtime).getInt())

    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putInt(dst, mult(MemoryOps.getInt(op1), MemoryOps.getInt(op2))))
  }

  final case class MC4(op1: MemoryOperand, op2: Int, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() * op2)

    override def write(writer: MethodVisitor): Unit =
      MemoryOps.putInt(dst, mult(MemoryOps.getInt(op1), const(op2)))
  }

  final case class MM8(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() * op2.materialize(runtime).getLong())

    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putLong(dst, mult(MemoryOps.getLong(op1), MemoryOps.getLong(op2))))
  }

  final case class MC8(op1: MemoryOperand, op2: Long, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() * op2)

    override def write(writer: MethodVisitor): Unit =
      MemoryOps.putLong(dst, mult(MemoryOps.getLong(op1), const(op2)))
  }

  def length4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int, dst: MemoryOperand): Execution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM4(op1, op2, dst)
      case (op1: MemoryOperand, op2: Int) => MC4(op1, op2, dst)
      case (op1: Int, op2: MemoryOperand) => MC4(op2, op1, dst)
      case (op1: Int, op2: Int) => Const.Write4(op1 * op2, dst)
    }

  def length8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long, dst: MemoryOperand): Execution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM8(op1, op2, dst)
      case (op1: MemoryOperand, op2: Long) => MC8(op1, op2, dst)
      case (op1: Long, op2: MemoryOperand) => MC8(op2, op1, dst)
      case (op1: Long, op2: Long) => Const.Write8(op1 * op2, dst)
    }
}

object Negate {
  final case class M4(op: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(-op.materialize(runtime).getInt())

    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putInt(dst, negate(MemoryOps.getInt(op))))
  }

  final case class M8(op: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(-op.materialize(runtime).getLong())

    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putLong(dst, negate(MemoryOps.getLong(op))))
  }

  def length4(opUnion: MemoryOperand | Int, dst: MemoryOperand): Execution = opUnion match {
    case op: MemoryOperand => M4(op, dst)
    case op: Int => Const.Write4(-op, dst)
  }

  def length8(opUnion: MemoryOperand | Long, dst: MemoryOperand): Execution = opUnion match {
    case op: MemoryOperand => M8(op, dst)
    case op: Long => Const.Write8(-op, dst)
  }
}

object Set {
  final case class M4(src: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(src.materialize(runtime).getInt())
    override def write(writer: MethodVisitor): Unit =
      MemoryOps.putInt(dst, MemoryOps.getInt(src))
  }

  final case class M8(src: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(src.materialize(runtime).getLong())
    override def write(writer: MethodVisitor): Unit =
      MemoryOps.putLong(dst, MemoryOps.getLong(src))
  }

  def length4(srcUnion: MemoryOperand | Int, dst: MemoryOperand): Execution = srcUnion match {
    case src: MemoryOperand => M4(src, dst)
    case src: Int => Const.Write4(src, dst)
  }

  def length8(srcUnion: MemoryOperand | Long, dst: MemoryOperand): Execution = srcUnion match {
    case src: MemoryOperand => M8(src, dst)
    case src: Long => Const.Write8(src, dst)
  }
}
