package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Bytes, Const, Fn, Operation, OperationWithSource}
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}
import com.tabishev.leshy.runtime.{CommonSymbols, Memory, MemoryRef, Runtime, RuntimeOps, StackMemory, Symbol}

import java.io.File
import java.nio.ByteBuffer
import java.util
import scala.collection.mutable

class Interpreter(loader: RoutineLoader, debug: Boolean) {
  private val runtime = new Runtime()
  private val loadedFunctions = mutable.HashMap[String, Fn]()

  private val constInterpreter = ConstInterpreter(runtime)
  import constInterpreter._

  def run(name: String)(init: StackMemory => Unit): Bytes = {
    assert(runtime.stack.size == 0)
    init(runtime.stack)
    try {
      run(name, 0)
      assert(runtime.stack.frameOffset == 0)
      Bytes.fromBytes(runtime.stack.getCurrentStackFrame())
    } finally {
      runtime.stack.clean()
    }
  }

  private def run(name: String, depth: Int): Unit = {
    val fn = loadedFunctions.getOrElse(name, {
      val loaded = loader.load(name).getOrElse(throw new IllegalArgumentException(s"can't load $name"))
      runtime.symbols.register(loaded.name)
      loaded.labels.foreach { case (name, _) => runtime.symbols.register(name) }
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

  private def run(op: OperationWithSource, depth: Int): Option[Symbol] = {
    if (debug) println("\t".repeat(depth) + s"${runtime.stack.frameToString}: ${op.op}")
    op.op match {
      case Operation.Extend(lengthAst) =>
        val length = evalConst(lengthAst).asInt
        runtime.stack.extend(length)
        None
      case Operation.Shrink(lengthAst) =>
        val length = evalConst(lengthAst).asInt
        runtime.stack.shrink(length)
        None
      case Operation.Call(offsetConst, targetConst) =>
        val offsetChange = evalConst(offsetConst).asInt
        val target = evalSymbol(targetConst)

        val prevOffset = runtime.stack.frameOffset
        val newOffset = if (offsetChange >= 0) runtime.stack.frameOffset + offsetChange else runtime.stack.size + offsetChange
        runtime.stack.offset(newOffset)
        run(target.name, depth + 1)
        runtime.stack.offset(prevOffset)
        None
      case Operation.CheckSize(length) =>
        assert(evalConst(length).asInt == runtime.stack.stackFrameSize())
        None
      case Operation.Branch(modifier, length, op1, op2, target) =>
        val lengthE = evalConst(length).asInt
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
      case Operation.Add(length, op1, op2, dst) =>
        val lengthE = evalConst(length).asInt
        RuntimeOps.add(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
        markConst(dst, lengthE, isConst = checkConst(op1, lengthE) && checkConst(op2, lengthE))
        None
      case Operation.Mult(length, op1, op2, dst) =>
        val lengthE = evalConst(length).asInt
        RuntimeOps.mult(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
        markConst(dst, lengthE, isConst = checkConst(op1, lengthE) && checkConst(op2, lengthE))
        None
      case Operation.Neg(length, op, dst) =>
        val lengthE = evalConst(length).asInt
        RuntimeOps.neg(lengthE, constOrAddressRef(op, lengthE), addressRef(dst))
        markConst(dst, lengthE, isConst = checkConst(op, lengthE))
        None
      case Operation.NotSpecialize(lengthAst, dstAst) =>
        val length = evalConst(lengthAst).asInt
        markConst(dstAst, length, isConst = false)
        None
      case Operation.Set(length, src, dst) =>
        val lengthE = evalConst(length).asInt
        RuntimeOps.set(lengthE, constOrAddressRef(src, lengthE), constOrAddressRef(dst, lengthE))
        markConst(dst, lengthE, isConst = checkConst(src, lengthE))
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
      runtime.stack.getRef(evalConst(address).asInt)
    case _ =>
      throw new UnsupportedOperationException(s"unsupported address: $address")
  }
}
