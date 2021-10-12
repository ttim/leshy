package com.tabishev.leshy.ast

import java.nio.{ByteBuffer, ByteOrder}
import java.util.Base64
import scala.util.Try

enum Const {
  case Literal(bytes: Array[Byte])
  case Stack(from: Const, length: Const)

  override def toString(): String = this match {
    case Const.Literal(bytes) =>
      val possible = Seq(
        asString.map { value => s"'$value'"},
        asInt.map { value => s"$value"},
        asLong.map { value => s"${value}_L"}
      ).flatten
      if (possible.isEmpty) asBase64Bytes.get else possible.mkString("/")
    case Const.Stack(from, length) =>
      "${" + from + ", " + length + "}"
  }

  def asInt: Option[Int] = asByteBuffer.flatMap { bytes =>
    if (bytes.limit() == 4) Some(bytes.getInt()) else None
  }

  def asLong: Option[Long] = asByteBuffer.flatMap { bytes =>
    if (bytes.limit() == 8) Some(bytes.getLong()) else None
  }

  def asBase64Bytes: Option[String] = asBytes.map { bytes =>
    Base64.getEncoder.encodeToString(bytes)
  }

  def asString: Option[String] = asBytes.flatMap { bytes =>
    Try { new String(bytes) }.filter { s =>
      s.forall { c => Character.isDigit(c) || Character.isAlphabetic(c) || c == '_' || c == '-' }
    }.toOption
  }

  def asByteBuffer: Option[ByteBuffer] =
    asBytes.map { bytes =>
      val bb = ByteBuffer.wrap(bytes)
      bb.order(ByteOrder.LITTLE_ENDIAN)
      bb
    }

  def asBytes: Option[Array[Byte]] = this match {
    case Const.Literal(bytes) => Some(bytes)
    case Const.Stack(_, _) => None
  }
}
