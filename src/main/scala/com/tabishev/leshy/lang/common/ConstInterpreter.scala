package com.tabishev.leshy.lang.common

import com.tabishev.leshy.lang.ast.{Address, Const, ConstOrAddress}
import com.tabishev.leshy.runtime.{Bytes, FrameOffset}

trait ConstInterpreter {
  def frameSize(): Int
  def symbols(): Symbols

  def isConst(from: FrameOffset, length: Int): Boolean
  def get(from: FrameOffset, length: Int): Array[Byte]

  final def evalOffset(const: Const): FrameOffset = frameOffset(evalConst(const).asInt)

  final def evalLength(const: Const): Int = evalConst(const).asInt

  final def evalConst(const: Const): Bytes = const match {
    case Const.Literal(bytes) =>
      bytes
    case Const.Symbol(name) =>
      symbols().resolve(name).asBytes
    case Const.Stack(fromBytes, lengthBytes) =>
      val from = frameOffset(fromBytes.asInt)
      val length = lengthBytes.asInt
      assert(isConst(from, length))
      Bytes.fromBytes(get(from, length))
  }

  final def checkConst(constOrAddress: ConstOrAddress, length: Int): Boolean =
    tryConst(constOrAddress, length).isDefined

  final def tryConst(constOrAddress: ConstOrAddress, length: Int): Option[Bytes] = constOrAddress match {
    case ConstOrAddress.Const(const) =>
      Some(evalConst(const).expand(length))
    case ConstOrAddress.Address(Address.Native(_)) =>
      None
    case ConstOrAddress.Address(Address.Stack(offsetAst)) =>
      val offset = evalOffset(offsetAst)
      if (isConst(offset, length)) {
        Some(Bytes.fromBytes(get(offset, length)))
      } else None
    case ConstOrAddress.Address(Address.StackOffset(_, _, _)) =>
      ???
  }

  final def evalSymbol(const: Const): Symbol = symbols().resolveById(evalConst(const).asInt)

  private def frameOffset(rawOffset: Int): FrameOffset = FrameOffset.maybeNegative(rawOffset, frameSize())
}
