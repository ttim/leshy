package com.tabishev.leshy.ast

import java.nio.file.Path

enum Address {
  // Address in stack corresponding to `address`
  // #const
  case Stack(address: Const)

  // Address in stack corresponding to 4 bytes in stack starting from `offset` after `base`, and must be less than `limit`
  // #[const, const, const]
  // while `base` and `limit` correspond to actual addresses on stack,
  // `offset` is correspond to stack position, where 4 bytes describe offset
  case StackOffset(address: Const, limit: Const, offset: Const)

  // Address in native memory corresponding to 8 bytes in stack starting from `stackOffset`
  // *const
  case Native(stackOffset: Const)
}

enum Operation {
  // Stack operations
  case Extend(length: Const)
  case Shrink(length: Const)
  case CheckSize(length: Const)
  // appends 4 constant bytes at the end equal to current length
  case AppendSize()
  case Append(bytes: Const)

  // Control flow operations
  case Branch(modifier: Const, length: Const, op1: Const | Address, op2: Const | Address, target: Const)
  case Jump(target: Const)

  // Call operations
  case Call(offset: Const, target: Const)

  // Constant operations
  case NonConst(length: Const, dst: Address)

  // Memory operations
  // Should be more different `Set` operations, to cover use cases with offsets and array offsets
  case Set(length: Const, src: Const | Address, dst: Address)
  // `length` is treated as 8 bytes
  case SetNative(length: Address, src: Const | Address.Native, dst: Address.Native)

  // Integer arithmetic operations
  case Add(length: Const, op1: Const | Address, op2: Const | Address, dst: Address)
  case Mult(length: Const, op1: Const | Address, op2: Const | Address, dst: Address)
  case Neg(length: Const, op: Const | Address, dst: Address)

  // Float arithmetic operations, length can be either 4 or 8
  case AddF(length: Const, op1: Const | Address, op2: Const | Address, dst: Address)

  // "Syscalls" operations
  // If `length` passed as address it's treated as 8 (?) bytes
  case Print(length: Const | Address, src: Const | Address)
  // TODO: Should be reimplemented in stdlib through Convertion & Print
  // Good exercise
  case PrintInt(length: Const, src: Address)
}

case class Origin(path: Path, line: Int)
case class OperationWithSource(op: Operation, origin: Origin)

case class Fn(
               name: String,
               ops: Seq[OperationWithSource],
               labels: Map[String, Int], // label name to offset
             )
