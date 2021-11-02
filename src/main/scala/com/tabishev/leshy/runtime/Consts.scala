package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes

import java.nio.{ByteBuffer, ByteOrder}

// shouldn't depend on stack
final class ConstsHolder(val stack: StackMemory) {
  private val marks: Memory = Memory.ofSize(100000, ro = false)

  def isConst(offset: FrameOffset, length: Int): Boolean = {
    assert(offset.get + length <= stack.frameSize())
    marks.allEquals(stack.getFrameOffset() + offset.get, length, 1)
  }

  def get(): Consts =
    Consts.fromMap(marks.equalOffsets(stack.getFrameOffset(), stack.frameSize(), 1).map { offset =>
      (offset - stack.getFrameOffset(), stack.memory.getByte(offset))
    }.toMap)

  def markConst(offset: FrameOffset, length: Int, isConst: Boolean): Unit = {
    assert(offset.get + length <= stack.frameSize())
    marks.fill(stack.getFrameOffset() + offset.get, length, if (isConst) 1 else 0)
  }
}

final case class Consts private (constOffsetsAndData: Bytes) {
  override def toString: String = {
    val sorted = asMap().toSeq.sortBy(_._1)
    s"[${sorted.map(_._1).mkString(", ")}] -> [${sorted.map(_._2).mkString(", ")}]"
  }

  def length: Int = constOffsetsAndData.length() / 5

  def asMap(): Map[Int, Byte] = (0 until length).map { index =>
    val offset = constOffsetsAndData.asByteBuffer.getInt(index * 5)
    val value = constOffsetsAndData.asByteBuffer.get(index * 5 + 4)
    (offset, value)
  }.toMap

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Consts] &&
    obj.asInstanceOf[Consts].constOffsetsAndData == constOffsetsAndData
}

object Consts {
  def fromMap(offsetToValue: Map[Int, Byte]): Consts = {
    val offsetsAndData = Array.fill[Byte](offsetToValue.size * 5)(0)
    val mirror = ByteBuffer.wrap(offsetsAndData).order(ByteOrder.LITTLE_ENDIAN)
    offsetToValue.toSeq.sortBy(_._1).zipWithIndex.foreach { case ((offset, value), index) =>
      mirror.putInt(index * 5, offset)
      mirror.put(index * 5 + 4, value)
    }
    Consts(Bytes.fromBytes(offsetsAndData))
  }

  def fnCall(callsite: Consts, offset: Int, calleeResult: Consts): Consts = {
    val caller = callsite.asMap().filter(_._1 < offset)
    val callee = calleeResult.asMap().map { (calleeOffset, result) => (calleeOffset + offset, result) }
    Consts.fromMap(caller ++ callee)
  }
}