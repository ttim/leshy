package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.Runtime

sealed abstract class Execution {
  def execute(runtime: Runtime): Unit
  def markConsts(runtime: Runtime): Unit
}

sealed abstract class NonConstExecution extends Execution {
  val length: Int
  val dst: MemoryOperand

  final def markConsts(runtime: Runtime): Unit = dst.markConst(runtime, length, isConst = false)
}

sealed abstract class NonConstExecution4 extends NonConstExecution {
  final val length: Int = 4
}

sealed abstract class NonConstExecution8 extends NonConstExecution {
  final val length: Int = 8
}

object Const {
  final case class Write4(value: Int, dst: MemoryOperand) extends Execution {
    override def execute(runtime: Runtime): Unit = dst.materialize(runtime).putInt(value)
    override def markConsts(runtime: Runtime): Unit = dst.markConst(runtime, 4, isConst = true)
  }

  final case class Write8(value: Long, dst: MemoryOperand) extends Execution {
    override def execute(runtime: Runtime): Unit = dst.materialize(runtime).putLong(value)
    override def markConsts(runtime: Runtime): Unit = dst.markConst(runtime, 8, isConst = true)
  }
}

object Mark {
  // Specialize can't implemented simalry because execution assumes spec ctx not changing between runs
  final case class NotSpecialize(length: Int, dst: MemoryOperand) extends NonConstExecution {
    override def execute(runtime: Runtime): Unit = ()
  }
}

object Stack {
  final case class SetSize(oldSize: Int, newSize: Int) extends Execution {
    override def execute(runtime: Runtime): Unit = runtime.stack.setFramesize(newSize)
    override def markConsts(runtime: Runtime): Unit =
      if (newSize > oldSize) runtime.consts.markConst(oldSize, newSize - oldSize, isConst = true)
  }
}

object Sum {
  // MM - memory, memory
  final case class MM4(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() + op2.materialize(runtime).getInt())
  }

  final case class MC4(op1: MemoryOperand, op2: Int, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() + op2)
  }

  final case class MM8(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() + op2.materialize(runtime).getLong())
  }

  final case class MC8(op1: MemoryOperand, op2: Long, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() + op2)
  }

  def length4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int, dst: MemoryOperand): Execution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM4(op1, op2, dst)
      case (op1: MemoryOperand, op2: Int) => MC4(op1, op2, dst)
      case (op1: Int, op2: MemoryOperand) => MC4(op2, op1, dst)
      case (op1: Int, op2: Int) => Const.Write4(op1 + op2, dst)
    }

  def length8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long, dst: MemoryOperand): Execution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM8(op1, op2, dst)
      case (op1: MemoryOperand, op2: Long) => MC8(op1, op2, dst)
      case (op1: Long, op2: MemoryOperand) => MC8(op2, op1, dst)
      case (op1: Long, op2: Long) => Const.Write8(op1 + op2, dst)
    }
}

object Mult {
  // MM - memory, memory
  final case class MM4(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() * op2.materialize(runtime).getInt())
  }

  final case class MC4(op1: MemoryOperand, op2: Int, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() * op2)
  }

  final case class MM8(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() * op2.materialize(runtime).getLong())
  }

  final case class MC8(op1: MemoryOperand, op2: Long, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() * op2)
  }

  def length4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int, dst: MemoryOperand): Execution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM4(op1, op2, dst)
      case (op1: MemoryOperand, op2: Int) => MC4(op1, op2, dst)
      case (op1: Int, op2: MemoryOperand) => MC4(op2, op1, dst)
      case (op1: Int, op2: Int) => Const.Write4(op1 * op2, dst)
    }

  def length8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long, dst: MemoryOperand): Execution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM8(op1, op2, dst)
      case (op1: MemoryOperand, op2: Long) => MC8(op1, op2, dst)
      case (op1: Long, op2: MemoryOperand) => MC8(op2, op1, dst)
      case (op1: Long, op2: Long) => Const.Write8(op1 * op2, dst)
    }
}

object Negate {
  final case class M4(op: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(-op.materialize(runtime).getInt())
  }

  final case class M8(op: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(-op.materialize(runtime).getLong())
  }

  def length4(opUnion: MemoryOperand | Int, dst: MemoryOperand): Execution = opUnion match {
    case op: MemoryOperand => M4(op, dst)
    case op: Int => Const.Write4(-op, dst)
  }

  def length8(opUnion: MemoryOperand | Long, dst: MemoryOperand): Execution = opUnion match {
    case op: MemoryOperand => M8(op, dst)
    case op: Long => Const.Write8(-op, dst)
  }
}

object Set {
  final case class M4(src: MemoryOperand, dst: MemoryOperand) extends NonConstExecution4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(src.materialize(runtime).getInt())
  }

  final case class M8(src: MemoryOperand, dst: MemoryOperand) extends NonConstExecution8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(src.materialize(runtime).getLong())
  }

  def length4(srcUnion: MemoryOperand | Int, dst: MemoryOperand): Execution = srcUnion match {
    case src: MemoryOperand => M4(src, dst)
    case src: Int => Const.Write4(src, dst)
  }

  def length8(srcUnion: MemoryOperand | Long, dst: MemoryOperand): Execution = srcUnion match {
    case src: MemoryOperand => M8(src, dst)
    case src: Long => Const.Write8(src, dst)
  }
}
