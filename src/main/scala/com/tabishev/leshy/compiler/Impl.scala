package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{MemoryRef, Runtime}

abstract class SimpleImpl {
  val dst: MemoryOperand
  val dstLength: Int
  val isConst: Boolean = false

  def execute(runtime: Runtime): Unit
}

trait Length4 {
  val dstLength: Int = 4
}

trait Length8 {
  val dstLength: Int = 8
}

abstract class BranchImpl {
  def execute(runtime: Runtime): Boolean
  def ifTrue: OperationRef
}

object Const {
  case class Write4(value: Int, dst: MemoryOperand) extends SimpleImpl {
    override val dstLength: Int = 4
    override val isConst: Boolean = true

    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(value)
  }

  case class Write8(value: Long, dst: MemoryOperand) extends SimpleImpl {
    override val dstLength: Int = 8
    override val isConst: Boolean = true

    override def execute(runtime: Runtime): Unit = dst.materialize(runtime).putLong(value)
  }
}

object Sum {
  // MM - memory, memory
  case class MM4(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends SimpleImpl with Length4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() + op2.materialize(runtime).getInt())
  }

  case class MC4(op1: MemoryOperand, op2: Int, dst: MemoryOperand) extends SimpleImpl with Length4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() + op2)
  }

  case class MM8(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends SimpleImpl with Length8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() + op2.materialize(runtime).getLong())
  }

  case class MC8(op1: MemoryOperand, op2: Long, dst: MemoryOperand) extends SimpleImpl with Length8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() + op2)
  }
}

object Negate {
  case class M4(op: MemoryOperand, dst: MemoryOperand) extends SimpleImpl with Length4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op.materialize(runtime).getInt())
  }

  case class M8(op: MemoryOperand, dst: MemoryOperand) extends SimpleImpl with Length8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op.materialize(runtime).getLong())
  }
}

object Branch {
  case class MoreMM4(op1: MemoryOperand, op2: MemoryOperand, dest: OperationRef) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getInt() > op2.materialize(runtime).getInt()

    override def ifTrue: OperationRef = dest
  }

  case class MoreMC4(op1: MemoryOperand, op2: Int, dest: OperationRef) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getInt() > op2

    override def ifTrue: OperationRef = dest
  }

  case class MoreMM8(op1: MemoryOperand, op2: MemoryOperand, dest: OperationRef) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getLong() > op2.materialize(runtime).getLong()

    override def ifTrue: OperationRef = dest
  }

  case class MoreMC8(op1: MemoryOperand, op2: Long, dest: OperationRef) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getLong() > op2

    override def ifTrue: OperationRef = dest
  }
}