package com.tabishev.leshy.ast

import java.nio.charset.Charset
import java.nio.{ByteBuffer, ByteOrder}
import java.util.Base64
import scala.util.Try

enum Const {
  case Literal(value: Bytes)
  case Stack(fromOffset: Bytes, length: Bytes)

  override def toString(): String = this match {
    case Const.Literal(bytes) => s"$bytes"
    case Const.Stack(from, length) => "${" + from + ", " + length + "}"
  }
}

object Bytes {
  val Empty: Bytes = Bytes.fromBytes(Array.emptyByteArray)

  def fromInt(value: Int): Bytes = {
    val buffer = Array.fill[Byte](4)(0)
    val bb = ByteBuffer.wrap(buffer)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(value)
    Bytes(buffer)
  }

  def fromLong(value: Long): Bytes = {
    val buffer = Array.fill[Byte](8)(0)
    val bb = ByteBuffer.wrap(buffer)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putLong(value)
    Bytes(buffer)
  }

  def fromBytes(bytes: Array[Byte]): Bytes = Bytes(bytes.clone())

  def fromString(s: String): Bytes = Bytes(s.getBytes(Charset.forName("UTF-8")))

  def seq(bs: Bytes*): Bytes = Bytes.fromBytes(bs.flatMap(_.get()).toArray)
}

case class Bytes(private val bytes: Array[Byte]) {
  lazy val asInt: Option[Int] =
    if (bytes.length == 4) Some(asByteBuffer.getInt()) else None
  lazy val asLong: Option[Long] =
    if (bytes.length == 8) Some(asByteBuffer.getLong()) else None

  def get(): Array[Byte] = bytes.clone()

  override def toString(): String = {
    val possible = Seq(
      asString.map { value => s"'$value'"},
      asInt.map { value => s"$value"},
      asLong.map { value => s"${value}_L"}
    ).flatten
    if (possible.isEmpty) asBase64Bytes else possible.mkString("/")
  }

  def slice(from: Int, until: Int): Bytes = Bytes(bytes.slice(from, until))
  def slice(from: Int): Bytes = Bytes(bytes.slice(from, bytes.length))

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Bytes] &&
    java.util.Arrays.equals(bytes, obj.asInstanceOf[Bytes].bytes)

  def asBase64Bytes: String =
    Base64.getEncoder.encodeToString(bytes)

  def asString: Option[String] =
    Try {
      new String(bytes)
    }.filter { s =>
      s.forall { c => Character.isDigit(c) || Character.isAlphabetic(c) || c == '_' || c == '-' }
    }.toOption

  private def asByteBuffer: ByteBuffer = {
    val bb = ByteBuffer.wrap(bytes)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb
  }
}

