package com.tabishev.leshy.lang.common

import com.tabishev.leshy.runtime.{Bytes, FrameOffset}

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

final case class Consts(offsetToValue: Map[Int, Byte]) {
  override def toString: String = {
    val sorted = offsetToValue.toSeq.sortBy(_._1)
    s"[${sorted.map(_._1).mkString(", ")}] -> [${sorted.map(_._2).mkString(", ")}]"
  }

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Consts] &&
    obj.asInstanceOf[Consts].offsetToValue == offsetToValue

  def isConst(offset: FrameOffset, length: Int): Boolean =
    (0 until length).forall { index => offsetToValue.contains(offset.get + index) }

  def get(offset: FrameOffset, length: Int): Array[Byte] =
    (0 until length).map { index => offsetToValue(offset.get + index) }.toArray

  def markConsts(offset: FrameOffset, bytes: Array[Byte]): Consts = {
    val result = mutable.HashMap.from(offsetToValue)
    bytes.indices.foreach { index =>
      result.put(offset.get + index, bytes(index))
    }
    Consts(result.toMap)
  }

  def unmarkConsts(offset: FrameOffset, length: Int): Consts = {
    val result = mutable.HashMap.from(offsetToValue)
    (0 until length).foreach { index => result.remove(offset.get + index) }
    Consts(result.toMap)
  }

  def call(offset: FrameOffset): Consts = Consts(
    offsetToValue.filter(_._1 >= offset.get).map { (calleeOffset, result) => (calleeOffset - offset.get, result) }
  )

  def returnFromCall(offset: FrameOffset, calleeResult: Consts): Consts = {
    val caller = offsetToValue.filter(_._1 < offset.get)
    val callee = calleeResult.offsetToValue.map { (calleeOffset, result) => (calleeOffset + offset.get, result) }
    Consts(caller ++ callee)
  }
}

object Consts {
  val Empty: Consts = Consts(Map())
}
