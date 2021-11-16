package com.tabishev.leshy.compiler

import com.tabishev.leshy.bytecode.BytecodeExpression
import com.tabishev.leshy.bytecode.BytecodeExpression.{const, invokeVirtual, mult, negate, sum}
import com.tabishev.leshy.runtime.{Consts, FrameOffset, Runtime, StackMemory}
import org.objectweb.asm.MethodVisitor

import com.tabishev.leshy.bytecode.*

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
