package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.Bytes
import com.tabishev.leshy.runtime.FrameOffset

trait Node {
  def get(): Node.Kind
}

object Node {
  sealed trait Kind
  case class Run(command: Command, next: Node) extends Kind
  case class Branch(condition: Condition, ifTrue: Node, ifFalse: Node) extends Kind
  case class Call(offset: FrameOffset, call: Node, next: Node => Node) extends Kind
  case object Final extends Kind
}

case class Output(length: Int, dst: MemoryOperand)

sealed trait MemoryOperand
object MemoryOperand {
  case class Stack(offset: FrameOffset) extends MemoryOperand
  case class Native(stackOffset: FrameOffset) extends MemoryOperand
}

sealed trait Command {
  import Command._

  def output: Option[Output] = this match {
    case Noop => None
    case SetFramesize(_) => None
    case Sum(length, dst, _, _) => Some(Output(length, dst))
    case Mult(length, dst, _, _) => Some(Output(length, dst))
    case Negate(length, dst, _) => Some(Output(length, dst))
    case Set(length, dst, _) => Some(Output(length, dst))
  }
}

object Command {
  case object Noop extends Command
  case class SetFramesize(size: Int) extends Command

  case class Sum(length: Int, dst: MemoryOperand, op1: Either[Bytes, MemoryOperand], op2: Either[Bytes, MemoryOperand]) extends Command
  case class Mult(length: Int, dst: MemoryOperand, op1: Either[Bytes, MemoryOperand], op2: Either[Bytes, MemoryOperand]) extends Command

  case class Negate(length: Int, dst: MemoryOperand, op: Either[Bytes, MemoryOperand]) extends Command
  case class Set(length: Int, dst: MemoryOperand, op: Either[Bytes, MemoryOperand]) extends Command
}

sealed trait ConditionModifier
object ConditionModifier {
  case object GT extends ConditionModifier
  case object LE extends ConditionModifier
  case object EQ extends ConditionModifier
}

sealed trait Condition
object Condition {
  case class Const(flag: Boolean) extends Condition
  case class Binary(length: Int, op1: Either[Bytes, MemoryOperand], modifier: ConditionModifier, op2: Either[Bytes, MemoryOperand]) extends Condition
}
