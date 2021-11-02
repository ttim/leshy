package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Bytes, Const}
import com.tabishev.leshy.runtime.{StackMemory, Symbol, Symbols, Runtime}

final case class ConstInterpreter(runtime: Runtime) {
  def frameSize(): Int = runtime.stack.frameSize()

  def evalConst(const: Const): Bytes = const match {
    case Const.Literal(bytes) =>
      bytes
    case Const.Symbol(name) =>
      runtime.symbols.resolve(name).asBytes
    case Const.Stack(fromBytes, lengthBytes) =>
      val from = fromBytes.asInt
      val length = lengthBytes.asInt
      assert(runtime.consts.isConst(from, length))
      Bytes.fromBytes(runtime.stack.getRef(from).get(length))
  }

  def checkConst(constOrAddress: Const | Address, length: Int): Boolean =
    tryConst(constOrAddress, length).isDefined

  def tryConst(constOrAddress: Const | Address, length: Int): Option[Bytes] = constOrAddress match {
    case const : Const =>
      Some(evalConst(const).expand(length))
    case Address.Native(_) =>
      None
    case Address.Stack(offsetAst) =>
      val offset = evalConst(offsetAst).asInt
      if (runtime.consts.isConst(offset, length)) {
        Some(Bytes.fromBytes(runtime.stack.getRef(offset).get(length)))
      } else None
    case Address.StackOffset(_, _, _) =>
      ???
  }

  def markConst(dst: Address, length: Int, isConst: Boolean): Unit = dst match {
    case Address.Stack(offset) =>
      runtime.consts.markConst(evalConst(offset).asInt, length, isConst)
    case Address.Native(_) =>
    // do nothing
    case Address.StackOffset(_, _, _) =>
      ???
  }

  def evalSymbol(const: Const): Symbol =
    runtime.symbols.resolveBytes(evalConst(const))
}
