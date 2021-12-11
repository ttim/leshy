package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.Bytes

object Unify {
  def command(command: Command): Option[Command.Set] = command match {
    case Command.Sum(length, dst, MemoryOperandOrBytes.Bytes(op1), MemoryOperandOrBytes.Bytes(op2)) =>
      Some(Command.Set(length, dst, MemoryOperandOrBytes.Bytes(sum(length, op1, op2))))
    case Command.Mult(length, dst, MemoryOperandOrBytes.Bytes(op1), MemoryOperandOrBytes.Bytes(op2)) =>
      Some(Command.Set(length, dst, MemoryOperandOrBytes.Bytes(mult(length, op1, op2))))
    case Command.Set(length, dst, MemoryOperandOrBytes.Bytes(op)) =>
      Some(Command.Set(length, dst, MemoryOperandOrBytes.Bytes(op)))
    case Command.Negate(length, dst, MemoryOperandOrBytes.Bytes(op)) =>
      Some(Command.Set(length, dst, MemoryOperandOrBytes.Bytes(negate(length, op))))
    case _ =>
      None
  }

  private def sum(length: Int, op1: Bytes, op2: Bytes): Bytes = length match {
    case 4 => Bytes.fromInt(op1.asInt + op2.asInt)
    case 8 => Bytes.fromLong(op1.asLong + op2.asLong)
  }

  private def mult(length: Int, op1: Bytes, op2: Bytes): Bytes = length match {
    case 4 => Bytes.fromInt(op1.asInt * op2.asInt)
    case 8 => Bytes.fromLong(op1.asLong * op2.asLong)
  }

  private def negate(length: Int, op: Bytes): Bytes = length match {
    case 4 => Bytes.fromInt(-op.asInt)
    case 8 => Bytes.fromLong(-op.asLong)
  }
}
