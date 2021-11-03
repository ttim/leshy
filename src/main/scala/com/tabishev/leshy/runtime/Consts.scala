package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

// shouldn't depend on stack
final class ConstsHolder(stack: StackMemory) {
  private var consts: Consts = Consts.fromMap(Map())

  def isConst(offset: FrameOffset, length: Int): Boolean =
    get().isConst(offset, length)

  def get(): Consts = consts

  def set(consts: Consts): Unit = this.consts = consts

  def call(offset: Int): Consts = {
    val prevConsts = consts
    consts = Consts.call(consts, offset)
    prevConsts
  }

  def returnFromCall(offset: Int, prev: Consts): Unit =
    consts = Consts.returnFromCall(prev, offset, consts)

  def markConsts(offset: FrameOffset, bytes: Array[Byte]): Unit =
    consts = consts.markConsts(offset, bytes)

  def unmarkConsts(offset: FrameOffset, length: Int): Unit =
    consts = consts.unmarkConsts(offset, length)
}

final case class Consts private (constOffsetsAndData: Bytes) {
  lazy val asMap: Map[Int, Byte] = (0 until length).map { index =>
    val offset = constOffsetsAndData.asByteBuffer.getInt(index * 5)
    val value = constOffsetsAndData.asByteBuffer.get(index * 5 + 4)
    (offset, value)
  }.toMap

  override def toString: String = {
    val sorted = asMap.toSeq.sortBy(_._1)
    s"[${sorted.map(_._1).mkString(", ")}] -> [${sorted.map(_._2).mkString(", ")}]"
  }

  def length: Int = constOffsetsAndData.length() / 5

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Consts] &&
    obj.asInstanceOf[Consts].constOffsetsAndData == constOffsetsAndData

  def isConst(offset: FrameOffset, length: Int): Boolean =
    (0 until length).forall { index => asMap.contains(offset.get + index) }

  def markConsts(offset: FrameOffset, bytes: Array[Byte]): Consts = {
    val result = mutable.HashMap.from(asMap)
    bytes.indices.foreach { index =>
      result.put(offset.get + index, bytes(index))
    }
    Consts.fromMap(result.toMap)
  }

  def unmarkConsts(offset: FrameOffset, length: Int): Consts = {
    val result = mutable.HashMap.from(asMap)
    (0 until length).foreach { index => result.remove(offset.get + index) }
    Consts.fromMap(result.toMap)
  }
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

  def returnFromCall(callsite: Consts, offset: Int, calleeResult: Consts): Consts = {
    val caller = callsite.asMap.filter(_._1 < offset)
    val callee = calleeResult.asMap.map { (calleeOffset, result) => (calleeOffset + offset, result) }
    Consts.fromMap(caller ++ callee)
  }

  def call(callsite: Consts, offset: Int): Consts = Consts.fromMap(
    callsite.asMap.filter(_._1 >= offset).map { (calleeOffset, result) => (calleeOffset - offset, result) }
  )
}