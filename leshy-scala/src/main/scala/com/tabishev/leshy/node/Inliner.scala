package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{Bytes, FrameOffset}

object Inliner {
  def inline(call: Node): Node = call.get() match {
    case call@Node.Call(_, node, _) => InlinedNode(call, node)
    case _ => throw new IllegalArgumentException(call + " isn't Call node")
  }

  case class InlinedNode private (call: Node.Call, node: Node) extends Node {
    override def get(): Node.Kind = node.get() match {
      case Node.Run(command, next) =>
        Node.Run(offsetCommand(call.offset, command), wrap(next))
      case Node.Branch(condition, ifTrue, ifFalse) =>
        Node.Branch(offsetCondition(call.offset, condition), wrap(ifTrue), wrap(ifFalse))
      case Node.Call(innerOffset, innerCall, innerNext) =>
        Node.Call(call.offset.plus(innerOffset), innerCall, innerNext.andThen(wrap))
      case Node.Final =>
        Node.Run(Command.Noop, call.next(node))
    }

    private def wrap(node: Node): Node = InlinedNode(call, node)
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
