package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.Bytes
import com.tabishev.leshy.runtime.FrameOffset

trait Node

object Node {
  trait Run extends Node {
    def command: Command

    def next: Node
  }

  trait Branch extends Node {
    def condition: Condition

    def ifTrue: Node
    def ifFalse: Node
  }

  trait Call extends Node {
    def offset: FrameOffset

    def call: Node
    def next(returnNode: Node.Final): Node
  }

  trait Final extends Node
}

case class Output(length: Int, dst: MemoryOperand)

enum MemoryOperand {
  case Stack(offset: FrameOffset)
  case Native(stackOffset: FrameOffset)
}

enum Command {
  case Noop
  case SetFramesize(size: Int)

  case Sum(length: Int, dst: MemoryOperand, op1: MemoryOperand | Bytes, op2: MemoryOperand | Bytes)
  case Mult(length: Int, dst: MemoryOperand, op1: MemoryOperand | Bytes, op2: MemoryOperand | Bytes)

  case Negate(length: Int, dst: MemoryOperand, op: MemoryOperand | Bytes)
  case Set(length: Int, dst: MemoryOperand, op: MemoryOperand | Bytes)

  def output: Option[Output] = this match {
    case Noop => None
    case SetFramesize(_) => None
    case Sum(length, dst, _, _) => Some(Output(length, dst))
    case Mult(length, dst, _, _) => Some(Output(length, dst))
    case Negate(length, dst, _) => Some(Output(length, dst))
    case Set(length, dst, _) => Some(Output(length, dst))
  }
}

enum Condition {
  case Const(flag: Boolean)
  case Gt(length: Int, op1: MemoryOperand | Bytes, op2: MemoryOperand | Bytes)
  case Le(length: Int, op1: MemoryOperand | Bytes, op2: MemoryOperand | Bytes)
  case Eq(length: Int, op1: MemoryOperand | Bytes, op2: MemoryOperand | Bytes)
}
