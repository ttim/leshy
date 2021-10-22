package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.Runtime

import java.util.concurrent.atomic.AtomicReference

class NodeContext(val loader: RoutineLoader) {
  private def operation(op: OperationRef): Option[ast.Operation] = {
    val ops = loader.load(op.fn).get.ops
    if (ops.length == op.line) None else Some(ops(op.line))
  }

  def create(runtime: Runtime, op: OperationRef): Node = {
    val specializationContext = SpecializationContext.current(runtime)
    operation(op) match {
      case None => ???
      case Some(operation) => operation match {
        case ast.Operation.Extend(lengthAst) => ???
        case ast.Operation.Shrink(lengthAst) => ???
        case ast.Operation.Append(bytesAst) => ???
        case ast.Operation.Call(offsetConst, targetConst) => ???
        case ast.Operation.CheckSize(length) => ???
        case ast.Operation.Branch(modifier, length, op1, op2, target) => ???
        case ast.Operation.Jump(target) => ???
        case ast.Operation.PrintInt(length, src) => ???
        case ast.Operation.Add(length, op1, op2, dst) => ???
        case ast.Operation.Mult(length, op1, op2, dst) => ???
        case ast.Operation.Neg(length, op, dst) => ???
        case ast.Operation.Set(length, src, dst) => ???
        case _ =>
          throw new IllegalArgumentException(s"unsupported operation '$op''")
      }
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

  final def run(runtime: Runtime): Unit = {
    var node = this
    while (!node.isInstanceOf[Node.Final]) node = node.runInternal(runtime)
  }

  protected def runInternal(runtime: Runtime): Node

  protected def nextLineNode(runtime: Runtime): Node = {
    if (computedNextNode == null)
      computedNextNode = nodeCtx.create(runtime, OperationRef(op.fn, op.line + 1))
    computedNextNode
  }
}

object Node {
  case class Simple(nodeCtx: NodeContext, specializationCtx: SpecializationContext, op: OperationRef, operation: Operation.Simple) extends Node {
    protected def runInternal(runtime: Runtime): Node = {
      operation.execute(runtime)
      nextLineNode(runtime)
    }
  }

  case class Noop(nodeCtx: NodeContext, specializationCtx: SpecializationContext, op: OperationRef) extends Node {
    protected def runInternal(runtime: Runtime): Node = nextLineNode(runtime)
  }

  case class Final(nodeCtx: NodeContext, specializationCtx: SpecializationContext, op: OperationRef) extends Node {
    protected def runInternal(runtime: Runtime): Node = null
  }
}
