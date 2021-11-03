package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Bytes, Const, Fn, Operation, OperationWithSource}
import com.tabishev.leshy.common.ConstInterpreter
import com.tabishev.leshy.loader.{FileLoader, FnLoader}
import com.tabishev.leshy.runtime._

import java.io.File
import java.nio.ByteBuffer
import java.util
import scala.collection.mutable

class Interpreter(loader: FnLoader, debug: Boolean, checkConsts: Boolean) extends ConstInterpreter {
  private val runtime = new Runtime()
  private var consts = Consts.Empty

  def run[T, V](spec: FnSpec[T, V])(input: T): V = {
    val inputObj = spec.input(input)

    assert(runtime.stack.isEmpty())
    runtime.stack.append(inputObj.bytes)
    updateConsts(_ => inputObj.consts)
    try {
      run(spec.fn, 0)
      assert(runtime.stack.getFrameOffset() == 0)
      spec.output(Bytes.fromBytes(runtime.stack.currentStackFrame()))
    } finally {
      runtime.stack.clean()
      updateConsts(_ => Consts.Empty)
    }
  }

  // for rest
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
    if (debug) println("\t".repeat(depth) + s"${runtime.stack.frameToString(consts)}: ${op.op}")
    op.op match {
      case Operation.Extend(lengthAst) =>
        val length = evalConst(lengthAst).asInt
        runtime.stack.extend(length)
        updateConsts(_.markConsts(FrameOffset.nonNegative(runtime.stack.frameSize() - length), Array.fill(length)(0)))
        None
      case Operation.Shrink(lengthAst) =>
        val length = evalConst(lengthAst).asInt
        runtime.stack.shrink(length)
        None
      case Operation.Call(offsetConst, targetConst) =>
        val newOffset = runtime.stack.offset(evalConst(offsetConst).asInt)
        val target = evalSymbol(targetConst)
        runtime.stack.moveFrame(newOffset.get)
        val callerConsts = updateConsts(_.call(newOffset))
        run(target.name, depth + 1)
        updateConsts { callee => callerConsts.returnFromCall(newOffset, callee) }
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

  // const related logic
  private def markConst(dst: Address, length: Int, isConst: Boolean): Unit = dst match {
    case Address.Stack(offsetAst) =>
      val offset = runtime.stack.offset(evalConst(offsetAst).asInt)
      if (isConst)
        updateConsts(_.markConsts(offset, runtime.stack.getRef(offset).get(length)))
      else
        updateConsts(_.unmarkConsts(offset, length))
    case Address.Native(_) =>
      // do nothing
    case Address.StackOffset(_, _, _) =>
      ???
  }

  override def frameSize(): Int = runtime.stack.frameSize()

  override def symbols(): Symbols = runtime.symbols

  override def get(from: FrameOffset, length: Int): Array[Byte] = runtime.stack.getRef(from).get(length)

  override def isConst(from: FrameOffset, length: Int): Boolean =
    !checkConsts || consts.isConst(from, length)

  private def updateConsts(fn: Consts => Consts): Consts =
    if (checkConsts) {
      val prev = consts
      consts = fn(consts)
      prev
    } else consts
}
