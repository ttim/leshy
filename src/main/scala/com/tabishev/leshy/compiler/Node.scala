package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Address, Bytes}
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.{MemoryRef, Runtime}

import java.util.concurrent.atomic.AtomicReference
import scala.util.{Failure, Success, Try}

enum MemoryOperand {
  case Stack(offset: Int)
  case Native(stackOffset: Int)

  def materialize(runtime: Runtime): MemoryRef = this match {
    case MemoryOperand.Stack(offset) => runtime.stack.getRef(offset)
    case MemoryOperand.Native(offset) => ???
  }

  def markConst(runtime: Runtime, length: Int, isConst: Boolean) = this match {
    case MemoryOperand.Stack(offset) =>
      runtime.stack.markConst(offset, length, isConst)
    case MemoryOperand.Native(offset) =>
    // do nothing
  }
}

case class OperationRef(fn: String, line: Int)

class NodeContext(val loader: RoutineLoader, val runtime: Runtime) {
  private val constInterpreter = ConstInterpreter(runtime)

  private def operation(op: OperationRef): Option[ast.Operation] = {
    val ops = loader.load(op.fn).get.ops
    if (ops.length == op.line) None else Some(ops(op.line))
  }

  def create(op: OperationRef): Node = {
    val specializationContext = SpecializationContext.current(runtime)

    def simpleNode(impl: SimpleImpl): Node =
      Node.Simple(this, specializationContext, op, impl, markConst = true)

    operation(op) match {
      case None => ???
      case Some(operation) => operation match {
        case ast.Operation.Extend(lengthAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          Node.StackResize(this, specializationContext, op, length)
        case ast.Operation.Shrink(lengthAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          Node.StackResize(this, specializationContext, op, -length)
        case ast.Operation.Append(bytesAst) => ???
        case ast.Operation.Call(offsetConst, targetConst) => ???
        case ast.Operation.CheckSize(lengthAst) =>
          Node.Throw(this, specializationContext, op, Try {
            runtime.stack.checkSize(constInterpreter.evalConst(lengthAst).asExpandedInt.get)
          })
        case ast.Operation.Branch(modifier, length, op1, op2, target) => ???
        case ast.Operation.Jump(target) => ???
        case ast.Operation.PrintInt(length, src) => ???
        case ast.Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => (toIntOrOperand(op1Ast), toIntOrOperand(op2Ast)) match {
              case (v1: Int, v2: Int) => Const.Write4(v1 + v2, dst)
              case (v1: Int, v2: MemoryOperand) => Sum.MC4(v2, v1, dst)
              case (v1: MemoryOperand, v2: Int) => Sum.MC4(v1, v2, dst)
              case (v1: MemoryOperand, v2: MemoryOperand) => Sum.MM4(v1, v2, dst)
            }
            case 8 => (toLongOrOperand(op1Ast), toLongOrOperand(op2Ast)) match {
              case (v1: Long, v2: Long) => Const.Write8(v1 + v2, dst)
              case (v1: Long, v2: MemoryOperand) => Sum.MC8(v2, v1, dst)
              case (v1: MemoryOperand, v2: Long) => Sum.MC8(v1, v2, dst)
              case (v1: MemoryOperand, v2: MemoryOperand) => Sum.MM8(v1, v2, dst)
            }
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.Mult(length, op1, op2, dst) => ???
        case ast.Operation.Neg(lengthAst, opAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => toIntOrOperand(opAst) match {
              case v: Int => Const.Write4(-v, dst)
              case v: MemoryOperand => Negate.M4(v, dst)
            }
            case 8 => toLongOrOperand(opAst) match {
              case v: Long => Const.Write8(-v, dst)
              case v: MemoryOperand => Negate.M8(v, dst)
            }
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.Set(lengthAst, srcAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = (srcAst, length) match {
            case (srcConst: ast.Const, 4) =>
              Const.Write4(constInterpreter.evalConst(srcConst).asExpandedInt.get, dst)
            case (srcConst: ast.Const, 8) =>
              Const.Write8(constInterpreter.evalConst(srcConst).asExpandedLong.get, dst)
            case _ => ???
          }
          simpleNode(impl)
        case _ =>
          throw new IllegalArgumentException(s"unsupported operation '$op''")
      }
    }
  }

  private def toOperand(address: ast.Address): MemoryOperand = address match {
    case ast.Address.Stack(offset) =>
      MemoryOperand.Stack(constInterpreter.evalConst(offset).asExpandedInt.get)
    case ast.Address.StackOffset(_, _, _) =>
      ???
    case ast.Address.Native(_) =>
      ???
  }

  private def toIntOrOperand(addressOrConst: ast.Address | ast.Const): Int | MemoryOperand =
    toBytesOrOperand(addressOrConst, 4, _.asInt.get)

  private def toLongOrOperand(addressOrConst: ast.Address | ast.Const): Long | MemoryOperand =
    toBytesOrOperand(addressOrConst, 8, _.asLong.get)

  private def toBytesOrOperand[T](addressOrConst: ast.Address | ast.Const, length: Int, transform: Bytes => T): T | MemoryOperand =
    addressOrConst match {
      case const: ast.Const =>
        transform(constInterpreter.evalConst(const).expand(length))
      case address: ast.Address =>
        constInterpreter.tryConst(address, length) match {
          case Some(bytes) => transform(bytes)
          case None => toOperand(address)
        }
    }
}

case class SpecializationContext(stackSize: Int, consts: Map[Int, Byte])

object SpecializationContext {
  def current(runtime: Runtime): SpecializationContext =
    SpecializationContext(runtime.stack.stackFrameSize(), runtime.stack.stackFrameConsts())
}

abstract class Node {
  private var computedNextNode: Node = null

  val nodeCtx: NodeContext
  val specializationCtx: SpecializationContext
  val op: OperationRef

  final def checkContext(): Boolean = specializationCtx == SpecializationContext.current(nodeCtx.runtime)

  final def run(): Unit = {
    assert(checkContext())
    var node = this
    while (!node.isInstanceOf[Node.Final]) node = node.runInternal()
  }

  protected def runInternal(): Node

  protected def nextLineNode(): Node = {
    if (computedNextNode == null)
      computedNextNode = nodeCtx.create(OperationRef(op.fn, op.line + 1))
    computedNextNode
  }
}

object Node {
  case class Simple(nodeCtx: NodeContext, specializationCtx: SpecializationContext, op: OperationRef,
                    impl: SimpleImpl, markConst: Boolean) extends Node {
    protected def runInternal(): Node = {
      impl.execute(nodeCtx.runtime)
      if (markConst) impl.dst.markConst(nodeCtx.runtime, impl.dstLength, impl.isConst)
      nextLineNode()
    }
  }

  case class Noop(nodeCtx: NodeContext, specializationCtx: SpecializationContext, op: OperationRef) extends Node {
    protected def runInternal(): Node = nextLineNode()
  }

  case class Final(nodeCtx: NodeContext, specializationCtx: SpecializationContext, op: OperationRef) extends Node {
    protected def runInternal(): Node = null
  }

  case class Throw(nodeCtx: NodeContext, specializationCtx: SpecializationContext, op: OperationRef, ex: Try[Unit]) extends Node {
    protected def runInternal(): Node = ex match {
      case Success(_) => nextLineNode()
      case Failure(exception) => throw exception
    }
  }

  case class StackResize(nodeCtx: NodeContext, specializationCtx: SpecializationContext, op: OperationRef, delta: Int) extends Node {
    protected def runInternal(): Node = {
      if (delta >= 0) nodeCtx.runtime.stack.extend(delta) else nodeCtx.runtime.stack.shrink(-delta)
      nextLineNode()
    }
  }
}
