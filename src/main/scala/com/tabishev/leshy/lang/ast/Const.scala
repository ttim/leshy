package com.tabishev.leshy.lang.ast

import com.tabishev.leshy.runtime.Bytes

import java.nio.charset.Charset
import java.nio.{ByteBuffer, ByteOrder}
import java.util.Base64
import scala.util.Try

sealed trait Const {
  override def toString(): String = this match {
    case Const.Literal(bytes) => s"$bytes"
    case Const.Symbol(name) => s":$name"
    case Const.Stack(from, length) => "${" + from + ", " + length + "}"
  }
}

object Const {
  case class Literal(value: Bytes) extends Const
  case class Stack(fromOffset: Bytes, length: Bytes) extends Const
  case class Symbol(name: String) extends Const // get resolved to 4 bytes during execution
}
