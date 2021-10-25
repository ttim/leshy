package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{MemoryRef, Runtime}

abstract class BranchExecution {
  def execute(runtime: Runtime): Boolean
}

object BranchExecution {
  case object Always extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = true
  }

  case object Never extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = false
  }

  def fromFlag(flag: Boolean): BranchExecution = if (flag) Always else Never

  case class MoreMM4(op1: MemoryOperand, op2: MemoryOperand) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getInt() > op2.materialize(runtime).getInt()
  }

  case class MoreMC4(op1: MemoryOperand, op2: Int) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getInt() > op2
  }

  case class MoreMM8(op1: MemoryOperand, op2: MemoryOperand) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getLong() > op2.materialize(runtime).getLong()
  }

  case class MoreMC8(op1: MemoryOperand, op2: Long) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getLong() > op2
  }

  case class LessOrEqualMM4(op1: MemoryOperand, op2: MemoryOperand) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getInt() <= op2.materialize(runtime).getInt()
  }

  case class LessOrEqualMC4(op1: MemoryOperand, op2: Int) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getInt() <= op2
  }

  case class LessOrEqualMM8(op1: MemoryOperand, op2: MemoryOperand) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getLong() <= op2.materialize(runtime).getLong()
  }

  case class LessOrEqualMC8(op1: MemoryOperand, op2: Long) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getLong() <= op2
  }

  def more4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int): BranchExecution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MoreMM4(op1, op2)
      case (op1: MemoryOperand, op2: Int) => MoreMC4(op1, op2)
      case (op1: Int, op2: MemoryOperand) => LessOrEqualMC4(op2, op1)
      case (op1: Int, op2: Int) => fromFlag(op1 > op2)
    }

  def more8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long): BranchExecution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MoreMM8(op1, op2)
      case (op1: MemoryOperand, op2: Long) => MoreMC8(op1, op2)
      case (op1: Long, op2: MemoryOperand) => LessOrEqualMC8(op2, op1)
      case (op1: Long, op2: Long) => fromFlag(op1 > op2)
    }

  def lessOrEqual4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int): BranchExecution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MoreMM4(op2, op1)
      case (op1: MemoryOperand, op2: Int) => LessOrEqualMC4(op1, op2)
      case (op1: Int, op2: MemoryOperand) => MoreMC4(op2, op1)
      case (op1: Int, op2: Int) => fromFlag(op1 <= op2)
    }

  def lessOrEqual8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long): BranchExecution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MoreMM8(op2, op1)
      case (op1: MemoryOperand, op2: Long) => LessOrEqualMC8(op1, op2)
      case (op1: Long, op2: MemoryOperand) => MoreMC8(op2, op1)
      case (op1: Long, op2: Long) => fromFlag(op1 <= op2)
    }
}
