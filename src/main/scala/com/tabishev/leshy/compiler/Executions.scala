package com.tabishev.leshy.compiler

import com.tabishev.leshy.bytecode.BytecodeExpression
import com.tabishev.leshy.bytecode.BytecodeExpression.{const, invokeVirtual, mult, negate, sum}
import com.tabishev.leshy.runtime.{Consts, FrameOffset, Runtime, StackMemory}
import org.objectweb.asm.MethodVisitor

import com.tabishev.leshy.bytecode.*

object Mark {
  // Specialize can't implemented simalry because execution assumes spec ctx not changing between runs
  final case class NotSpecialize(dst: MemoryOperand, length: Int) extends Execution {
    override def execute(runtime: Runtime): Unit = ()
    override def write(writer: MethodVisitor): Unit = ()
    override def markConsts(stackSize: Int, consts: Consts): Consts = dst.unmarkConst(consts, length)
  }
}

object Stack {
  final case class SetSize(size: Int) extends Execution {
    override def execute(runtime: Runtime): Unit = runtime.stack.setFramesize(size)
    override def write(writer: MethodVisitor): Unit =
      writer.statement(invokeVirtual(classOf[StackMemory], "setFramesize", MemoryOps.Stack, const(size)))

    override def markConsts(oldSize: Int, consts: Consts): Consts = {
      if (size > oldSize)
        consts.markConsts(FrameOffset.nonNegative(oldSize), Array.fill[Byte](size - oldSize)(0))
      else
        consts.unmarkConsts(FrameOffset.nonNegative(size), oldSize - size)
    }

    override def stackSize(before: Int): Int = size
  }
}

object Sum {
  final case class Length4(dst: MemoryOperand, op1: IntProvider, op2: IntProvider) extends BinaryIntExecution {
    override def eval(arg1: Int, arg2: Int): Int = arg1 + arg2
    override val expression: BytecodeExpression = sum(op1.expression, op2.expression)
  }

  final case class Length8(dst: MemoryOperand, op1: LongProvider, op2: LongProvider) extends BinaryLongExecution {
    override def eval(arg1: Long, arg2: Long): Long = arg1 + arg2
    override val expression: BytecodeExpression = sum(op1.expression, op2.expression)
  }
}

object Mult {
  final case class Length4(dst: MemoryOperand, op1: IntProvider, op2: IntProvider) extends BinaryIntExecution {
    override def eval(arg1: Int, arg2: Int): Int = arg1 * arg2
    override val expression: BytecodeExpression = mult(op1.expression, op2.expression)
  }

  final case class Length8(dst: MemoryOperand, op1: LongProvider, op2: LongProvider) extends BinaryLongExecution {
    override def eval(arg1: Long, arg2: Long): Long = arg1 * arg2
    override val expression: BytecodeExpression = mult(op1.expression, op2.expression)
  }
}

object Negate {
  final case class Length4(dst: MemoryOperand, src: IntProvider) extends UnaryIntExecution {
    override def eval(arg: Int): Int = -arg
    override val expression: BytecodeExpression = negate(src.expression)
  }

  final case class Length8(dst: MemoryOperand, src: LongProvider) extends UnaryLongExecution {
    override def eval(arg: Long): Long = -arg
    override val expression: BytecodeExpression = negate(src.expression)
  }
}

object Set {
  final case class Length4(dst: MemoryOperand, src: IntProvider) extends UnaryIntExecution {
    override def eval(arg: Int): Int = arg
    override val expression: BytecodeExpression = src.expression
  }

  final case class Length8(dst: MemoryOperand, src: LongProvider) extends UnaryLongExecution {
    override def eval(arg: Long): Long = arg
    override val expression: BytecodeExpression = src.expression
  }
}
