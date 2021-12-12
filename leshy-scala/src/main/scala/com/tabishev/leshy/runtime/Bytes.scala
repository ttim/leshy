package com.tabishev.leshy.runtime

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.Charset
import java.util.Base64
import scala.util.Try

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
  lazy val asInt: Int =
    if (bytes.length == 4) asByteBuffer.getInt() else throw new IllegalArgumentException(s"not int $asString")
  lazy val asLong: Long =
    if (bytes.length == 8) asByteBuffer.getLong() else throw new IllegalArgumentException(s"not long $asString")

  lazy val asBase64Bytes: String =
    Base64.getEncoder.encodeToString(bytes)
  lazy val asString: Option[String] =
    Try {
      new String(bytes)
    }.filter { s =>
      s.forall { c => Character.isDigit(c) || Character.isAlphabetic(c) || c == '_' || c == '-' }
    }.toOption

  def get(): Array[Byte] = bytes.clone()

  def length(): Int = bytes.length

  def copyTo(dest: ByteBuffer, offset: Int): Unit =
    dest.put(offset, bytes, 0, bytes.length)

  def expand(length: Int): Bytes =
    if (bytes.length == length) this else {
      assert(length > bytes.length)
      val expanded = Array.fill[Byte](length)(0)
      System.arraycopy(bytes, 0, expanded, 0, bytes.length)
      Bytes(expanded)
    }

  override def toString(): String = {
    val possible = Seq(
      asString.map { value => s"'$value'"},
      Try { asInt }.toOption.map { value => s"$value"},
      Try { asLong }.toOption.map { value => s"${value}_L"}
    ).flatten
    if (possible.isEmpty) asBase64Bytes else possible.mkString("/")
  }

  def slice(from: Int, until: Int): Bytes = Bytes(bytes.slice(from, until))
  def slice(from: Int): Bytes = Bytes(bytes.slice(from, bytes.length))

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Bytes] &&
    java.util.Arrays.equals(bytes, obj.asInstanceOf[Bytes].bytes)

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

  def asByteBuffer: ByteBuffer = {
    val bb = ByteBuffer.wrap(bytes).asReadOnlyBuffer()
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb
  }
}
