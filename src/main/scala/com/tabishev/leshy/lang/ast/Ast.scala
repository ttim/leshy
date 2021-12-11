package com.tabishev.leshy.lang.ast

import java.nio.file.Path

sealed trait Address
object Address {
  // Address in stack corresponding to `address`
  // #const
  case class Stack(address: Const) extends Address

  // Address in stack corresponding to 4 bytes in stack starting from `offset` after `base`, and must be less than `limit`
  // #[const, const, const]
  // while `base` and `limit` correspond to actual addresses on stack,
  // `offset` is correspond to stack position, where 4 bytes describe offset
  case class StackOffset(address: Const, limit: Const, offset: Const) extends Address

  // Address in native memory corresponding to 8 bytes in stack starting from `stackOffset`
  // *const
  case class Native(stackOffset: Const) extends Address
}

sealed trait Operation
object Operation {
  // Stack operations
  case class Extend(length: Const) extends Operation
  case class Shrink(length: Const) extends Operation
  case class CheckSize(length: Const) extends Operation

  // Control flow operations
  case class Branch(modifier: Const, length: Const, op1: Const | Address, op2: Const | Address, target: Const) extends Operation
  case class Jump(target: Const) extends Operation

  // Call operations
  case class Call(offset: Const, target: Const) extends Operation

  // Constant operations
  case class Specialize(length: Const, dst: Address) extends Operation
  case class NotSpecialize(length: Const, dst: Address) extends Operation

  // Memory operations
  // Should be more different `Set` operations, to cover use cases with offsets and array offsets
  case class Set(length: Const, src: Const | Address, dst: Address) extends Operation
  // `length` is treated as 8 bytes
  case class SetNative(length: Address, src: Const | Address.Native, dst: Address.Native) extends Operation

  // Integer arithmetic operations
  case class Add(length: Const, op1: Const | Address, op2: Const | Address, dst: Address) extends Operation
  case class Mult(length: Const, op1: Const | Address, op2: Const | Address, dst: Address) extends Operation
  case class Neg(length: Const, op: Const | Address, dst: Address) extends Operation
}

case class Origin(path: Path, line: Int)
case class OperationWithSource(op: Operation, origin: Origin)

case class Fn(
               name: String,
               ops: Seq[OperationWithSource],
               labels: Map[String, Int], // label name to offset
             ) {
  override val hashCode: Int = super.hashCode()
}
