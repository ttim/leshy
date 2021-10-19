package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Bytes, Const, Fn, Operation}
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}

import java.io.File
import java.nio.ByteBuffer
import java.util
import scala.collection.mutable

class CommonSymbols(register: String => Symbol) {
  val eq = register("eq")
  val neq = register("neq")
  val m = register("m")
  val le = register("le")
}

class Interpreter(loader: RoutineLoader, debug: Boolean) {
  private val state = new Runtime()
  private val commonSymbols = CommonSymbols(state.symbols.register)
  private val loadedFunctions = mutable.HashMap[String, Fn]()

  def run(name: String, input: Bytes): Bytes = {
    assert(state.stack.size == 0)
    state.stack.append(input)
    try {
      run(name, 0)
      assert(state.stack.offset == 0)
      Bytes.fromBytes(state.stack.getFully())
    } finally {
      state.stack.shrink(state.stack.size)
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
      case Operation.Extend(length) =>
        state.stack.extend(evalConst(length).asExpandedInt.get)
        None
      case Operation.Shrink(length) =>
        state.stack.shrink(evalConst(length).asExpandedInt.get)
        None
      case Operation.Append(bytes) =>
        state.stack.append(evalConst(bytes))
        None
      case Operation.Call(offsetConst, targetConst) => {
        val offsetChange = evalConst(offsetConst).asExpandedInt.get
        val target = evalSymbol(targetConst)

        val prevOffset = state.stack.offset
        val newOffset = if (offsetChange >= 0) state.stack.offset + offsetChange else state.stack.size + offsetChange
        state.stack.offset(newOffset)
        run(target.name, depth + 1)
        state.stack.offset(prevOffset)
        None
      }
      case Operation.CheckSize(length) =>
        state.stack.checkSize(evalConst(length).asExpandedInt.get)
        None
      case Operation.Branch(modifier, length, op1, op2, target) => {
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
      }
      case Operation.Jump(target) =>
        Some(evalSymbol(target))
      case Operation.PrintInt(length, src) =>
        RuntimeOps.printInt(evalConst(length).asExpandedInt.get, addressRef(src))
        None
      case Operation.Add(length, op1, op2, dst) => {
        val lengthE = evalConst(length).asExpandedInt.get
        RuntimeOps.add(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
        None
      }
      case Operation.Mult(length, op1, op2, dst) => {
        val lengthE = evalConst(length).asExpandedInt.get
        RuntimeOps.mult(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
        None
      }
      case Operation.Neg(length, op, dst) => {
        val lengthE = evalConst(length).asExpandedInt.get
        RuntimeOps.neg(lengthE, constOrAddressRef(op, lengthE), addressRef(dst))
        None
      }
      case Operation.Set(length, src, dst) => {
        val lengthE = evalConst(length).asExpandedLong.get
        RuntimeOps.set(lengthE, constOrAddressRef(src, lengthE.toInt), constOrAddressRef(dst, lengthE.toInt))
        None
      }
      case _ =>
        throw new IllegalArgumentException(s"unsupported operation '$op''")
    }
  }

  private def constOrAddressRef(constOrAddress: Const | Address, constExpectedLength: Int): MemoryRef =
    constOrAddress match {
      case const: Const =>
        new MemoryRef(evalConst(const).expand(constExpectedLength).asByteBuffer, 0)
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
    case Const.Literal(bytes) => bytes
    case Const.Symbol(name) => state.symbols.resolve(name).asBytes
    case Const.Stack(from, length) => {
      // todo: check constantness
      val address = addressRef(Address.Stack(Const.Literal(from)))
      Bytes.fromBytes(address.getBytes(length.asInt.get))
    }
  }

  private def evalSymbol(const: Const): Symbol = state.symbols.resolveBytes(evalConst(const))
}
