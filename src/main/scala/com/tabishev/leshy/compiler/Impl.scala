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

object NonConst {
  case class Mark(length: Int, dst: MemoryOperand) extends SimpleImpl {
    override val dstLength: Int = length
    override def execute(runtime: Runtime): Unit =
      () // do nothing apart from marking purposes which is done in node
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

  def length4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int, dst: MemoryOperand): SimpleImpl =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM4(op1, op2, dst)
      case (op1: MemoryOperand, op2: Int) => MC4(op1, op2, dst)
      case (op1: Int, op2: MemoryOperand) => MC4(op2, op1, dst)
      case (op1: Int, op2: Int) => Const.Write4(op1 + op2, dst)
    }

  def length8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long, dst: MemoryOperand): SimpleImpl =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM8(op1, op2, dst)
      case (op1: MemoryOperand, op2: Long) => MC8(op1, op2, dst)
      case (op1: Long, op2: MemoryOperand) => MC8(op2, op1, dst)
      case (op1: Long, op2: Long) => Const.Write8(op1 + op2, dst)
    }
}

object Mult {
  // MM - memory, memory
  case class MM4(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends SimpleImpl with Length4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() * op2.materialize(runtime).getInt())
  }

  case class MC4(op1: MemoryOperand, op2: Int, dst: MemoryOperand) extends SimpleImpl with Length4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(op1.materialize(runtime).getInt() * op2)
  }

  case class MM8(op1: MemoryOperand, op2: MemoryOperand, dst: MemoryOperand) extends SimpleImpl with Length8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() * op2.materialize(runtime).getLong())
  }

  case class MC8(op1: MemoryOperand, op2: Long, dst: MemoryOperand) extends SimpleImpl with Length8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(op1.materialize(runtime).getLong() * op2)
  }

  def length4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int, dst: MemoryOperand): SimpleImpl =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM4(op1, op2, dst)
      case (op1: MemoryOperand, op2: Int) => MC4(op1, op2, dst)
      case (op1: Int, op2: MemoryOperand) => MC4(op2, op1, dst)
      case (op1: Int, op2: Int) => Const.Write4(op1 * op2, dst)
    }

  def length8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long, dst: MemoryOperand): SimpleImpl =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MM8(op1, op2, dst)
      case (op1: MemoryOperand, op2: Long) => MC8(op1, op2, dst)
      case (op1: Long, op2: MemoryOperand) => MC8(op2, op1, dst)
      case (op1: Long, op2: Long) => Const.Write8(op1 * op2, dst)
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

  def length4(opUnion: MemoryOperand | Int, dst: MemoryOperand): SimpleImpl = opUnion match {
    case op: MemoryOperand => M4(op, dst)
    case op: Int => Const.Write4(-op, dst)
  }

  def length8(opUnion: MemoryOperand | Long, dst: MemoryOperand): SimpleImpl = opUnion match {
    case op: MemoryOperand => M8(op, dst)
    case op: Long => Const.Write8(-op, dst)
  }
}

object Set {
  case class M4(src: MemoryOperand, dst: MemoryOperand) extends SimpleImpl with Length4 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putInt(src.materialize(runtime).getInt())
  }

  case class M8(src: MemoryOperand, dst: MemoryOperand) extends SimpleImpl with Length8 {
    override def execute(runtime: Runtime): Unit =
      dst.materialize(runtime).putLong(src.materialize(runtime).getLong())
  }

  def length4(srcUnion: MemoryOperand | Int, dst: MemoryOperand): SimpleImpl = srcUnion match {
    case src: MemoryOperand => M4(src, dst)
    case src: Int => Const.Write4(src, dst)
  }

  def length8(srcUnion: MemoryOperand | Long, dst: MemoryOperand): SimpleImpl = srcUnion match {
    case src: MemoryOperand => M8(src, dst)
    case src: Long => Const.Write8(src, dst)
  }
}

object Branch {
  case object Always extends BranchImpl {
    override def execute(runtime: Runtime): Boolean = true
  }

  case object Never extends BranchImpl {
    override def execute(runtime: Runtime): Boolean = false
  }

  def fromFlag(flag: Boolean): BranchImpl = if (flag) Always else Never

  case class MoreMM4(op1: MemoryOperand, op2: MemoryOperand) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getInt() > op2.materialize(runtime).getInt()
  }

  case class MoreMC4(op1: MemoryOperand, op2: Int) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getInt() > op2
  }

  case class MoreMM8(op1: MemoryOperand, op2: MemoryOperand) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getLong() > op2.materialize(runtime).getLong()
  }

  case class MoreMC8(op1: MemoryOperand, op2: Long) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getLong() > op2
  }

  case class LessOrEqualMM4(op1: MemoryOperand, op2: MemoryOperand) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getInt() <= op2.materialize(runtime).getInt()
  }

  case class LessOrEqualMC4(op1: MemoryOperand, op2: Int) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getInt() <= op2
  }

  case class LessOrEqualMM8(op1: MemoryOperand, op2: MemoryOperand) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getLong() <= op2.materialize(runtime).getLong()
  }

  case class LessOrEqualMC8(op1: MemoryOperand, op2: Long) extends BranchImpl {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getLong() <= op2
  }

  def more4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int): BranchImpl =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MoreMM4(op1, op2)
      case (op1: MemoryOperand, op2: Int) => MoreMC4(op1, op2)
      case (op1: Int, op2: MemoryOperand) => LessOrEqualMC4(op2, op1)
      case (op1: Int, op2: Int) => fromFlag(op1 > op2)
    }

  def more8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long): BranchImpl =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MoreMM8(op1, op2)
      case (op1: MemoryOperand, op2: Long) => MoreMC8(op1, op2)
      case (op1: Long, op2: MemoryOperand) => LessOrEqualMC8(op2, op1)
      case (op1: Long, op2: Long) => fromFlag(op1 > op2)
    }

  def lessOrEqual4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int): BranchImpl =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MoreMM4(op2, op1)
      case (op1: MemoryOperand, op2: Int) => LessOrEqualMC4(op1, op2)
      case (op1: Int, op2: MemoryOperand) => MoreMC4(op2, op1)
      case (op1: Int, op2: Int) => fromFlag(op1 <= op2)
    }

  def lessOrEqual8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long): BranchImpl =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => MoreMM8(op2, op1)
      case (op1: MemoryOperand, op2: Long) => LessOrEqualMC8(op1, op2)
      case (op1: Long, op2: MemoryOperand) => MoreMC8(op2, op1)
      case (op1: Long, op2: Long) => fromFlag(op1 <= op2)
    }
}
