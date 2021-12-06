package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.runtime.Consts
import com.tabishev.leshy.node.{Command, Condition, MemoryOperand, Unify}

sealed abstract class Execution {
  def command: Command

  // by default derive from command
  def specialize(before: SpecializationContext): SpecializationContext =
    SpecializationContext(before.stackSize, updateConsts(before.consts))

  private def updateConsts(consts: Consts): Consts = {
    val output = command.output.get
    Unify.command(command) match {
      case Some(Command.Set(length, dst, op: Bytes)) =>
        markConst(output.dst, consts, op.get())
      case Some(_) =>
        throw new IllegalStateException("unify suppose to return Command.Set with bytes")
      case None =>
        unmarkConst(output.dst, consts, output.length)
    }
  }

  private[compiler] def markConst(op: MemoryOperand, consts: Consts, bytes: Array[Byte]): Consts = op match {
    case MemoryOperand.Stack(offset) =>
      consts.markConsts(offset, bytes)
    case MemoryOperand.Native(offset) =>
      // do nothing
      consts
  }

  private[compiler] def unmarkConst(op: MemoryOperand, consts: Consts, length: Int): Consts = op match {
    case MemoryOperand.Stack(offset) =>
      consts.unmarkConsts(offset, length)
    case MemoryOperand.Native(offset) =>
      // do nothing
      consts
  }
}

object Executions {
  // Specialize can't implemented similarly because execution assumes spec ctx not changing between runs
  final case class NotSpecialize(dst: MemoryOperand, length: Int) extends Execution {
    override def command: Command = Command.Noop

    override def specialize(before: SpecializationContext): SpecializationContext =
      SpecializationContext(before.stackSize, unmarkConst(dst, before.consts, length))
  }

  final case class SetSize(size: Int) extends Execution {
    override def command: Command = Command.SetFramesize(size)

    override def specialize(before: SpecializationContext): SpecializationContext =
      before.setSize(size)
  }

  final case class Simple(command: Command) extends Execution
}
