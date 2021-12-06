package com.tabishev.leshy.lang.ast

import com.tabishev.leshy.runtime.Bytes

import java.nio.charset.Charset
import java.nio.{ByteBuffer, ByteOrder}
import java.util.Base64
import scala.util.Try

enum Const {
  case Literal(value: Bytes)
  case Stack(fromOffset: Bytes, length: Bytes)
  case Symbol(name: String) // get resolved to 4 bytes during execution

  override def toString(): String = this match {
    case Const.Literal(bytes) => s"$bytes"
    case Const.Symbol(name) => s":$name"
    case Const.Stack(from, length) => "${" + from + ", " + length + "}"
  }
}
