package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.runtime.{Consts, FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}
import com.tabishev.leshy.bytecode._
import com.tabishev.leshy.bytecode.BytecodeExpression._

sealed abstract class Execution {
  def execute(runtime: Runtime): Unit
  def write(writer: MethodVisitor): Unit

  def markConsts(consts: Consts): Consts
  def stackSize(before: Int): Int = before
}

sealed abstract class BinaryIntExecution extends Execution {
  val op1: IntProvider
  val op2: IntProvider
  val dst: MemoryOperand

  def eval(arg1: Int, arg2: Int): Int
  val expression: BytecodeExpression

  override final def execute(runtime: Runtime): Unit =
    dst.materialize(runtime).putInt(eval(op1.get(runtime), op2.get(runtime)))
  override final def write(writer: MethodVisitor): Unit =
    writer.statement(MemoryOps.putInt(dst, expression))

  def markConsts(consts: Consts): Consts = (op1, op2) match {
    case (IntProvider.Const(v1), IntProvider.Const(v2)) =>
      dst.markConst(consts, Bytes.fromInt(eval(v1, v2)).get())
    case _ =>
      dst.unmarkConst(consts, 4)
  }
}

sealed abstract class BinaryLongExecution extends Execution {
  val op1: LongProvider
  val op2: LongProvider
  val dst: MemoryOperand

  def eval(arg1: Long, arg2: Long): Long
  val expression: BytecodeExpression

  override final def execute(runtime: Runtime): Unit =
    dst.materialize(runtime).putLong(eval(op1.get(runtime), op2.get(runtime)))
  override final def write(writer: MethodVisitor): Unit =
    writer.statement(MemoryOps.putLong(dst, expression))

  def markConsts(consts: Consts): Consts = (op1, op2) match {
    case (LongProvider.Const(v1), LongProvider.Const(v2)) =>
      dst.markConst(consts, Bytes.fromLong(eval(v1, v2)).get())
    case _ =>
      dst.unmarkConst(consts, 8)
  }
}

sealed abstract class UnaryIntExecution extends Execution {
  val src: IntProvider
  val dst: MemoryOperand

  def eval(arg: Int): Int
  val expression: BytecodeExpression

  override final def execute(runtime: Runtime): Unit =
    dst.materialize(runtime).putInt(eval(src.get(runtime)))
  override final def write(writer: MethodVisitor): Unit =
    writer.statement(MemoryOps.putInt(dst, expression))

  def markConsts(consts: Consts): Consts = src match {
    case IntProvider.Const(v) =>
      dst.markConst(consts, Bytes.fromInt(eval(v)).get())
    case _ =>
      dst.unmarkConst(consts, 4)
  }
}

sealed abstract class UnaryLongExecution extends Execution {
  val src: LongProvider
  val dst: MemoryOperand

  def eval(arg: Long): Long
  val expression: BytecodeExpression

  override final def execute(runtime: Runtime): Unit =
    dst.materialize(runtime).putLong(eval(src.get(runtime)))
  override final def write(writer: MethodVisitor): Unit =
    writer.statement(MemoryOps.putLong(dst, expression))

  def markConsts(consts: Consts): Consts = src match {
    case LongProvider.Const(v) =>
      dst.markConst(consts, Bytes.fromLong(eval(v)).get())
    case _ =>
      dst.unmarkConst(consts, 8)
  }
}

object Mark {
  // Specialize can't implemented simalry because execution assumes spec ctx not changing between runs
  final case class NotSpecialize(length: Int, dst: MemoryOperand) extends Execution {
    override def execute(runtime: Runtime): Unit = ()
    override def write(writer: MethodVisitor): Unit = ()
    override def markConsts(consts: Consts): Consts = dst.unmarkConst(consts, length)
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
  final case class Length4(op1: IntProvider, op2: IntProvider, dst: MemoryOperand) extends BinaryIntExecution {
    override def eval(arg1: Int, arg2: Int): Int = arg1 + arg2
    override val expression: BytecodeExpression = sum(op1.expression, op2.expression)
  }

  final case class Length8(op1: LongProvider, op2: LongProvider, dst: MemoryOperand) extends BinaryLongExecution {
    override def eval(arg1: Long, arg2: Long): Long = arg1 + arg2
    override val expression: BytecodeExpression = sum(op1.expression, op2.expression)
  }
}

object Mult {
  final case class Length4(op1: IntProvider, op2: IntProvider, dst: MemoryOperand) extends BinaryIntExecution {
    override def eval(arg1: Int, arg2: Int): Int = arg1 * arg2
    override val expression: BytecodeExpression = mult(op1.expression, op2.expression)
  }

  final case class Length8(op1: LongProvider, op2: LongProvider, dst: MemoryOperand) extends BinaryLongExecution {
    override def eval(arg1: Long, arg2: Long): Long = arg1 * arg2
    override val expression: BytecodeExpression = mult(op1.expression, op2.expression)
  }
}

object Negate {
  final case class Length4(src: IntProvider, dst: MemoryOperand) extends UnaryIntExecution {
    override def eval(arg: Int): Int = -arg
    override val expression: BytecodeExpression = negate(src.expression)
  }

  final case class Length8(src: LongProvider, dst: MemoryOperand) extends UnaryLongExecution {
    override def eval(arg: Long): Long = -arg
    override val expression: BytecodeExpression = negate(src.expression)
  }
}

object Set {
  final case class Length4(src: IntProvider, dst: MemoryOperand) extends UnaryIntExecution {
    override def eval(arg: Int): Int = arg
    override val expression: BytecodeExpression = src.expression
  }

  final case class Length8(src: LongProvider, dst: MemoryOperand) extends UnaryLongExecution {
    override def eval(arg: Long): Long = arg
    override val expression: BytecodeExpression = src.expression
  }
}
