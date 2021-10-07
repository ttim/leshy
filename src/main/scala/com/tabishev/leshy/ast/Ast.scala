package com.tabishev.leshy.ast

enum Const {
  case Literal(bytes: Array[Byte])
  case Stack(from: Const, length: Const)
}

enum Address {
  // Address in stack corresponding to `address`
  case DirectStack(address: Const)

  // Address in stack corresponding to 4 bytes in stack starting from `stackOffset`, must be between `low` and `high`
  case IndirectStack(stackOffset: Const, low: Const, high: Const)

  // Address in heap corresponding to `address`
  case DirectHeap(address: Const)

  // Address in heap corresponding to 8 bytes in stack starting from `stackOffset`
  case IndirectHeap(stackOffset: Const)
}

enum Operation {
  // Stack operations
  case Extend(length: Const)
  case Shrink(length: Const)
  case CheckSize(length: Const)

  // Branch operations
  case Branch(modifier: Const, length: Const, op1: Const | Address, op2: Const | Address, target: Const)

  // Call operations
  case Call(offset: Const, target: Const)

  // Constant operations

  // Memory operations
  // If `length` passed as address it's treated as 8 (?) bytes
  case Copy(length: Const | Address, src: Const | Address, dst: Address)
  case Set(length: Const | Address, bytes: Const | Address, dst: Address)

  // Integer arithmetic operations
  case Sum(length: Const, op1: Const | Address, op2: Const | Address, dst: Address)

  // Float arithmetic operations, length can be either 4 or 8
  case SumF(length: Const, op1: Const | Address, op2: Const | Address, dst: Address)
}
