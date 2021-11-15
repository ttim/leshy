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

object WriteConst {
  final case class Length4(value: Int, dst: MemoryOperand) extends Execution {
    override def execute(runtime: Runtime): Unit = dst.materialize(runtime).putInt(value)
    override def write(writer: MethodVisitor): Unit = writer.statement(MemoryOps.putInt(dst, const(value)))

    override def markConsts(consts: Consts): Consts = dst.markConst(consts, Bytes.fromInt(value).get())
  }

  final case class Length8(value: Long, dst: MemoryOperand) extends Execution {
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
  final case class Length4(op1: IntProvider, op2: IntProvider, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.get(runtime) + op2.get(runtime))
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putInt(dst, sum(op1.expression, op2.expression)))
  }

  final case class Length8(op1: LongProvider, op2: LongProvider, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.get(runtime) + op2.get(runtime))
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putLong(dst, sum(op1.expression, op2.expression)))
  }

  def length4(op1: MemoryOperand | Int, op2: MemoryOperand | Int, dst: MemoryOperand): Execution =
    (op1, op2) match {
      // we need to treat this cases separately to keep constantness in a correct way
      case (op1: Int, op2: Int) => WriteConst.Length4(op1 + op2, dst)
      case _ => Length4(IntProvider.create(op1), IntProvider.create(op2), dst)
    }

  def length8(op1: MemoryOperand | Long, op2: MemoryOperand | Long, dst: MemoryOperand): Execution =
    (op1, op2) match {
      case (op1: Long, op2: Long) => WriteConst.Length8(op1 + op2, dst)
      case _ => Length8(LongProvider.create(op1), LongProvider.create(op2), dst)
    }
}

object Mult {
  final case class Length4(op1: IntProvider, op2: IntProvider, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.get(runtime) * op2.get(runtime))
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putInt(dst, mult(op1.expression, op2.expression)))
  }

  final case class Length8(op1: LongProvider, op2: LongProvider, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.get(runtime) * op2.get(runtime))
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putLong(dst, mult(op1.expression, op2.expression)))
  }

  def length4(op1: MemoryOperand | Int, op2: MemoryOperand | Int, dst: MemoryOperand): Execution =
    (op1, op2) match {
      case (op1: Int, op2: Int) => WriteConst.Length4(op1 * op2, dst)
      case _ => Length4(IntProvider.create(op1), IntProvider.create(op2), dst)
    }

  def length8(op1: MemoryOperand | Long, op2: MemoryOperand | Long, dst: MemoryOperand): Execution =
    (op1, op2) match {
      case (op1: Long, op2: Long) => WriteConst.Length8(op1 * op2, dst)
      case _ => Length8(LongProvider.create(op1), LongProvider.create(op2), dst)
    }
}

object Negate {
  final case class Length4(op: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(-op.materialize(runtime).getInt())

    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putInt(dst, negate(MemoryOps.getInt(op))))
  }

  final case class Length8(op: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(-op.materialize(runtime).getLong())

    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putLong(dst, negate(MemoryOps.getLong(op))))
  }

  def length4(opUnion: MemoryOperand | Int, dst: MemoryOperand): Execution =
    opUnion match {
      case op: MemoryOperand => Length4(op, dst)
      case op: Int => WriteConst.Length4(-op, dst)
    }

  def length8(opUnion: MemoryOperand | Long, dst: MemoryOperand): Execution =
    opUnion match {
      case op: MemoryOperand => Length8(op, dst)
      case op: Long => WriteConst.Length8(-op, dst)
    }
}

object Set {
  final case class Length4(src: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(src.materialize(runtime).getInt())
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putInt(dst, MemoryOps.getInt(src)))
  }

  final case class Length8(src: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(src.materialize(runtime).getLong())
    override def write(writer: MethodVisitor): Unit =
      writer.statement(MemoryOps.putLong(dst, MemoryOps.getLong(src)))
  }

  def length4(srcUnion: MemoryOperand | Int, dst: MemoryOperand): Execution =
    srcUnion match {
      case src: Int => WriteConst.Length4(src, dst)
      case src: MemoryOperand => Length4(src, dst)
    }

  def length8(srcUnion: MemoryOperand | Long, dst: MemoryOperand): Execution =
    srcUnion match {
      case src: Long => WriteConst.Length8(src, dst)
      case src: MemoryOperand => Length8(src, dst)
    }
}
