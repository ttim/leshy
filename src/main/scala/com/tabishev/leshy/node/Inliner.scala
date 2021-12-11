package com.tabishev.leshy.node
import com.tabishev.leshy.runtime.{Bytes, FrameOffset}

object Inliner {
  def inline(call: Node.Call): Node = wrap(call, call.call)

  private def wrap(call: Node.Call, node: Node): Node = node match {
    case node: Node.Run => InlinedRun(call, node)
    case node: Node.Branch => InlinedBranch(call, node)
    case node: Node.Call => InlinedCall(call, node)
    case node: Node.Final => call.next(node)
  }

  private case class InlinedRun(original: Node.Call, inner: Node.Run) extends Node.Run {
    override def command: Command = offsetCommand(original.offset, inner.command)
    override def next: Node = wrap(original, inner.next)
  }

  private case class InlinedBranch(original: Node.Call, inner: Node.Branch) extends Node.Branch {
    override def condition: Condition = offsetCondition(original.offset, inner.condition)
    override def ifTrue: Node = wrap(original, inner.ifTrue)
    override def ifFalse: Node = wrap(original, inner.ifFalse)
  }

  private case class InlinedCall(original: Node.Call, inner: Node.Call) extends Node.Call {
    override def offset: FrameOffset = original.offset.plus(inner.offset)
    override def call: Node = inner.call
    override def next(returnNode: Node.Final): Node = wrap(original, inner.next(returnNode))
  }

  private def offsetCommand(offset: FrameOffset, command: Command): Command = command match {
    case Command.Noop =>
      Command.Noop
    case Command.SetFramesize(size) =>
      assert(size >= 0)
      Command.SetFramesize(size + offset.get)
    case Command.Sum(length, dst, op1, op2) =>
      Command.Sum(length, offsetOp(offset, dst), offsetOpOrBytes(offset, op1), offsetOpOrBytes(offset, op2))
    case Command.Set(length, dst, op) =>
      Command.Set(length, offsetOp(offset, dst), offsetOpOrBytes(offset, op))
    case Command.Mult(length, dst, op1, op2) =>
      Command.Mult(length, offsetOp(offset, dst), offsetOpOrBytes(offset, op1), offsetOpOrBytes(offset, op2))
    case Command.Negate(length, dst, op) =>
      Command.Negate(length, offsetOp(offset, dst), offsetOpOrBytes(offset, op))
  }

  private def offsetCondition(offset: FrameOffset, condition: Condition): Condition = condition match {
    case Condition.Const(flag) =>
      Condition.Const(flag)
    case Condition.Binary(length, op1, modifier, op2) =>
      Condition.Binary(length, offsetOpOrBytes(offset, op1), modifier, offsetOpOrBytes(offset, op2))
  }

  private def offsetOp(offset: FrameOffset, op: MemoryOperand): MemoryOperand = op match {
    case MemoryOperand.Stack(address) => MemoryOperand.Stack(address.plus(offset))
    case MemoryOperand.Native(_) => ???
  }

  private def offsetOpOrBytes(offset: FrameOffset, opOrBytes: Either[Bytes, MemoryOperand]): Either[Bytes, MemoryOperand] =
    opOrBytes match {
      case Right(op) => Right(offsetOp(offset, op))
      case Left(bytes) => Left(bytes)
    }
}
