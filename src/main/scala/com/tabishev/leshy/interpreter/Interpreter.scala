package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Bytes, Const, Fn, Operation}
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}
import com.tabishev.leshy.runtime.{CommonSymbols, Memory, MemoryRef, Runtime, RuntimeOps, StackMemory, Symbol}

import java.io.File
import java.nio.ByteBuffer
import java.util
import scala.collection.mutable

class Interpreter(loader: RoutineLoader, debug: Boolean) {
  private val state = new Runtime()
  private val loadedFunctions = mutable.HashMap[String, Fn]()

  def run(name: String)(init: StackMemory => Unit): Bytes = {
    assert(state.stack.size == 0)
    init(state.stack)
    try {
      run(name, 0)
      assert(state.stack.frameOffset == 0)
      Bytes.fromBytes(state.stack.getCurrentStackFrame())
    } finally {
      state.stack.clean()
    }
  }

  private def run(name: String, depth: Int): Unit = {
    val fn = loadedFunctions.getOrElse(name, {
      val loaded = loader.load(name).getOrElse(throw new IllegalArgumentException(s"can't load $name"))
      state.symbols.register(loaded.name)
      loaded.labels.foreach { case (name, _) => state.symbols.register(name) }
      loaded
    })

    var i = 0
    while (i < fn.ops.length) {
      run(fn.ops(i), depth) match {
        case Some(toJump) =>
          i = fn.labels(toJump.name)
        case None =>
          i += 1
      }
    }
  }

  private def run(op: Operation, depth: Int): Option[Symbol] = {
    if (debug) println("\t".repeat(depth) + s"${state.stack}: $op")
    op match {
      case Operation.Extend(lengthAst) =>
        val length = evalConst(lengthAst).asExpandedInt.get
        state.stack.extend(length)
        None
      case Operation.Shrink(lengthAst) =>
        val length = evalConst(lengthAst).asExpandedInt.get
        state.stack.shrink(length)
        None
      case Operation.Append(bytesAst) =>
        val bytes = evalConst(bytesAst)
        state.stack.append(bytes, isConst = true)
        None
      case Operation.Call(offsetConst, targetConst) =>
        val offsetChange = evalConst(offsetConst).asExpandedInt.get
        val target = evalSymbol(targetConst)

        val prevOffset = state.stack.frameOffset
        val newOffset = if (offsetChange >= 0) state.stack.frameOffset + offsetChange else state.stack.size + offsetChange
        state.stack.offset(newOffset)
        run(target.name, depth + 1)
        state.stack.offset(prevOffset)
        None
      case Operation.CheckSize(length) =>
        state.stack.checkSize(evalConst(length).asExpandedInt.get)
        None
      case Operation.Branch(modifier, length, op1, op2, target) =>
        val lengthE = evalConst(length).asExpandedInt.get
        val modifierE = evalSymbol(modifier)
        val op1Ref = constOrAddressRef(op1, lengthE)
        val op2Ref = constOrAddressRef(op2, lengthE)
        val flag = modifierE.name match {
          case "eq" => RuntimeOps.equals(lengthE, op1Ref, op2Ref)
          case "neq" => !RuntimeOps.equals(lengthE, op1Ref, op2Ref)
          case "le" => RuntimeOps.less(lengthE, op1Ref, op2Ref, orEqual = true)
          case "m" => !RuntimeOps.less(lengthE, op1Ref, op2Ref, orEqual = true)
          case _ => throw new IllegalArgumentException(s"unsupported branch modifier '$modifierE''")
        }
        if (flag) Some(evalSymbol(target)) else None
      case Operation.Jump(target) =>
        Some(evalSymbol(target))
      case Operation.PrintInt(length, src) =>
        RuntimeOps.printInt(evalConst(length).asExpandedInt.get, addressRef(src))
        None
      case Operation.Add(length, op1, op2, dst) =>
        val lengthE = evalConst(length).asExpandedInt.get
        RuntimeOps.add(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
        markConst(dst, lengthE, isConst = checkConst(op1, lengthE) && checkConst(op2, lengthE))
        None
      case Operation.Mult(length, op1, op2, dst) =>
        val lengthE = evalConst(length).asExpandedInt.get
        RuntimeOps.mult(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
        markConst(dst, lengthE, isConst = checkConst(op1, lengthE) && checkConst(op2, lengthE))
        None
      case Operation.Neg(length, op, dst) =>
        val lengthE = evalConst(length).asExpandedInt.get
        RuntimeOps.neg(lengthE, constOrAddressRef(op, lengthE), addressRef(dst))
        markConst(dst, lengthE, isConst = checkConst(op, lengthE))
        None
      case Operation.Set(length, src, dst) =>
        val lengthE = evalConst(length).asExpandedLong.get
        RuntimeOps.set(lengthE, constOrAddressRef(src, lengthE.toInt), constOrAddressRef(dst, lengthE.toInt))
        markConst(dst, lengthE.toInt, isConst = checkConst(src, lengthE.toInt))
        None
      case _ =>
        throw new IllegalArgumentException(s"unsupported operation '$op''")
    }
  }

  private def constOrAddressRef(constOrAddress: Const | Address, constExpectedLength: Int): MemoryRef =
    constOrAddress match {
      case const: Const =>
        new MemoryRef(Memory.ofBytes(evalConst(const).expand(constExpectedLength).get(), ro = true), 0)
      case address: Address =>
        addressRef(address)
    }

  private def addressRef(address: Address): MemoryRef = address match {
    case Address.Stack(address) =>
      state.stack.getRef(evalConst(address).asExpandedInt.get)
    case _ =>
      throw new UnsupportedOperationException(s"unsupported address: $address")
  }

  // const evaluation
  private def evalConst(const: Const): Bytes = const match {
    case Const.Literal(bytes) =>
      bytes
    case Const.Symbol(name) =>
      state.symbols.resolve(name).asBytes
    case Const.Stack(fromBytes, lengthBytes) =>
      val from = fromBytes.asExpandedInt.get
      val length = lengthBytes.asExpandedInt.get
      assert(state.stack.isConst(from, length))
      Bytes.fromBytes(state.stack.getRef(from).get(length))
  }

  private def evalSymbol(const: Const): Symbol = state.symbols.resolveBytes(evalConst(const))

  private def checkConst(constOrAddress: Const | Address, length: Int): Boolean = constOrAddress match {
    case _ : Const =>
      true
    case Address.Native(_) =>
      false
    case Address.Stack(offset) =>
      state.stack.isConst(evalConst(offset).asExpandedInt.get, length)
    case Address.StackOffset(_, _, _) =>
      ???
  }

  private def markConst(dst: Address, length: Int, isConst: Boolean): Unit = dst match {
    case Address.Stack(offset) =>
      state.stack.markConst(evalConst(offset).asExpandedInt.get, length, isConst)
    case Address.Native(_) =>
      // do nothing
    case Address.StackOffset(_, _, _) =>
      ???
  }
}
