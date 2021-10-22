package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{MemoryRef, Runtime}

enum MemoryOperand {
  case Stack(offset: Int)
  case Native(stackOffset: Int)

  def materialize(runtime: Runtime): MemoryRef = this match {
    case MemoryOperand.Stack(offset) => runtime.stack.getRef(offset)
    case MemoryOperand.Native(offset) => ???
  }
}

case class OperationRef(fn: String, line: Int)

sealed trait Operation

object Operation {
  trait Simple extends Operation {
    def execute(runtime: Runtime): Unit
  }

  trait Branch extends Operation {
    def execute(runtime: Runtime): Boolean
    def ifTrue: OperationRef
  }

  case class Jump(dest: OperationRef) extends Operation
}

object Const {
  case class Write4(value: Int, dst: MemoryOperand) extends Operation.Simple {
    override def execute(runtime: Runtime): Unit = dst.materialize(runtime).putInt(value)
  }

  case class Write8(value: Long, dst: MemoryOperand) extends Operation.Simple {
    override def execute(runtime: Runtime): Unit = dst.materialize(runtime).putLong(value)
  }
}

object Sum {
  // MM - memory, memory
  case class MM4(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends Operation.Simple {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() + op2.materialize(runtime).getInt())
  }

  case class MC4(op1: MemoryOperand, op2: Int, dst: MemoryOperand) extends Operation.Simple {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() + op2)
  }

  case class MM8(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends Operation.Simple {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() + op2.materialize(runtime).getLong())
  }

  case class MC8(op1: MemoryOperand, op2: Long, dst: MemoryOperand) extends Operation.Simple {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() + op2)
  }
}

object Branch {
  case class MoreMM4(op1: MemoryOperand, op2: MemoryOperand, dest: OperationRef) extends Operation.Branch {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getInt() > op2.materialize(runtime).getInt()

    override def ifTrue: OperationRef = dest
  }

  case class MoreMC4(op1: MemoryOperand, op2: Int, dest: OperationRef) extends Operation.Branch {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getInt() > op2

    override def ifTrue: OperationRef = dest
  }

  case class MoreMM8(op1: MemoryOperand, op2: MemoryOperand, dest: OperationRef) extends Operation.Branch {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getLong() > op2.materialize(runtime).getLong()

    override def ifTrue: OperationRef = dest
  }

  case class MoreMC8(op1: MemoryOperand, op2: Long, dest: OperationRef) extends Operation.Branch {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getLong() > op2

    override def ifTrue: OperationRef = dest
  }
}