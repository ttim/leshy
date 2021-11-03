package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Bytes, Const, Fn, Operation, OperationWithSource}
import com.tabishev.leshy.loader.{FileLoader, FnLoader}
import com.tabishev.leshy.runtime.{CommonSymbols, FnSpec, FrameOffset, Memory, MemoryRef, Runtime, RuntimeOps, StackMemory, Symbol}

import java.io.File
import java.nio.ByteBuffer
import java.util
import scala.collection.mutable

class Interpreter(loader: FnLoader, debug: Boolean) {
  private val runtime = new Runtime()
  private val constInterpreter = ConstInterpreter(runtime)
  import constInterpreter._

  def run[T, V](spec: FnSpec[T, V])(input: T): V =
    spec.output(run(spec.fn)(stack => spec.input(input, stack)))

  def run(name: String)(init: Runtime => Unit): Bytes = {
    assert(runtime.stack.isEmpty())
    init(runtime)
    try {
      run(name, 0)
      assert(runtime.stack.getFrameOffset() == 0)
      Bytes.fromBytes(runtime.stack.currentStackFrame())
    } finally {
      runtime.stack.clean()
    }
  }

  private def run(name: String, depth: Int): Unit = {
    val fn = loader.load(name).get
    runtime.symbols.register(fn)

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
    if (debug) println("\t".repeat(depth) + s"${runtime.stack.frameToString(runtime.consts.get())}: ${op.op}")
    op.op match {
      case Operation.Extend(lengthAst) =>
        val length = evalConst(lengthAst).asInt
        runtime.stack.extend(length)
        runtime.consts.markConst(FrameOffset.nonNegative(runtime.stack.frameSize() - length), length, isConst = true)
        None
      case Operation.Shrink(lengthAst) =>
        val length = evalConst(lengthAst).asInt
        runtime.stack.shrink(length)
        None
      case Operation.Call(offsetConst, targetConst) =>
        val newOffset = runtime.stack.offset(evalConst(offsetConst).asInt)
        val target = evalSymbol(targetConst)
        runtime.stack.moveFrame(newOffset.get)
        val prevConsts = runtime.consts.call(newOffset.get)
        run(target.name, depth + 1)
        runtime.consts.returnFromCall(newOffset.get, prevConsts)
        runtime.stack.moveFrame(-newOffset.get)
        None
      case Operation.CheckSize(length) =>
        assert(evalConst(length).asInt == runtime.stack.frameSize())
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
      runtime.stack.getRef(runtime.stack.offset(evalConst(address).asInt))
    case _ =>
      throw new UnsupportedOperationException(s"unsupported address: $address")
  }
}
