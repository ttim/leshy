package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.*

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

final class Runtime {
  val stack = new StackMemory()
  val symbols = new Symbols()

  CommonSymbols.register(symbols)
}

case class Consts private (constOffsetsAndData: Bytes) {
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

class StackMemory {
  private val initialSize = 1000

  var memory: Memory = Memory.ofSize(initialSize, ro = false)
  var marks: Memory = Memory.ofSize(initialSize, ro = false)

  var size: Int = 0
  var frameOffset: Int = 0

  def stackFrameSize(): Int = size - frameOffset
  def getCurrentStackFrame(): Array[Byte] = memory.get(frameOffset, size - frameOffset)

  private def calcAbsoluteOffset(offset: Int): Int =
    if (offset >= 0) {
      assert(offset < (size - frameOffset))
      frameOffset + offset
    } else {
      assert((-offset) <= (size - frameOffset))
      size + offset
    }

  def getRef(offset: Int): MemoryRef = new MemoryRef(memory, calcAbsoluteOffset(offset))

  // offset >= 0 and guaranteed in range
  def getRefUnsafe(offset: Int): MemoryRef = new MemoryRef(memory, frameOffset + offset)

  def setFramesize(newFrameSize: Int): Unit = {
    assert(newFrameSize >= 0)

    val newSize = frameOffset + newFrameSize
    if (newSize > size) {
      // extend
      if (newSize > memory.size) {
        val memoryExtendSize = Math.min(memory.size, newSize - memory.size)
        memory = memory.extended(memoryExtendSize, ro = false)
        marks = marks.extended(memoryExtendSize, ro = false)
      }

      memory.fill(size, newSize - size, 0)
      size = newSize
    } else if (newSize < size) {
      // shrink
      size = newSize
    }
  }

  def extend(extendSize: Int): Unit = {
    assert(extendSize >= 0)
    setFramesize(stackFrameSize() + extendSize)
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(shrinkSize >= 0)
    setFramesize(stackFrameSize() - shrinkSize)
  }

  def clean(): Unit = {
    size = 0
    frameOffset = 0
  }

  def append(bytes: Bytes, isConst: Boolean): Unit = {
    extend(bytes.length())
    memory.putBytes(size - bytes.length(), bytes)
    markConst(size - bytes.length(), bytes.length(), isConst)
  }

  def offset(newOffset: Int): Unit = {
    assert(frameOffset >= 0)
    this.frameOffset = newOffset
  }

  def frameToString: String = {
    import scala.io.AnsiColor.RED
    import scala.io.AnsiColor.RESET

    val frameData = memory.get(frameOffset, size - frameOffset)
    val frameMarks = marks.get(frameOffset, size - frameOffset)
    frameData.zip(frameMarks).map { case (byte, mark) =>
      if (mark == 0) byte.toString else RED + byte.toString + RESET
    }.mkString(", ")
  }

  // const actions
  def markConst(offset: Int, length: Int, isConst: Boolean): Unit = {
    val absoluteOffset = calcAbsoluteOffset(offset)
    assert(absoluteOffset + length <= size)
    marks.fill(absoluteOffset, length, if (isConst) 1 else 0)
  }

  def isConst(offset: Int, length: Int): Boolean = {
    val absoluteOffset = calcAbsoluteOffset(offset)
    assert(absoluteOffset + length <= size)
    marks.allEquals(absoluteOffset, length, 1)
  }

  def stackFrameConsts(): Consts =
    Consts.fromMap(marks.equalOffsets(frameOffset, size - frameOffset, 1).map { offset =>
      (offset - frameOffset, memory.getByte(offset))
    }.toMap)
}