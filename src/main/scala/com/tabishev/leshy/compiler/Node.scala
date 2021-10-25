package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Address, Bytes, Fn, OperationWithSource, Origin}
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.{CommonSymbols, Consts, MemoryRef, Runtime, StackMemory}

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

abstract class Node {
  private var computedNextNode: Node = null

  val compiler: Compiler
  val ctx: SpecializationContext
  val op: OperationRef

  final def checkContext(): Unit =
    if (!compiler.frozen && compiler.debugEnabled) {
      assert(ctx == SpecializationContext.current(compiler.runtime))
    }

  final def run(): SpecializationContext = {
    checkContext()

    var node = this
    while (!node.isInstanceOf[Node.Final]) {
      compiler.debug(node.op, ctx, "start run")
      val nextNode = node.runInternal()
      compiler.debug(node.op, ctx, "finish run")
      node = nextNode
    }

    node.ctx
  }

  protected def runInternal(): Node

  protected def nextLineNode(): Node = {
    if (computedNextNode == null)
      // todo: to other nodes apart from simple original context can be used
      computedNextNode = compiler.create(op.next, SpecializationContext.current(compiler.runtime))
    computedNextNode
  }
}

trait UpdatesConst {
  var markConst: Boolean = true
}

object Node {
  case class Simple(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                    impl: SimpleImpl) extends Node with UpdatesConst {
    protected def runInternal(): Node = {
      impl.execute(compiler.runtime)
      if (markConst) impl.dst.markConst(compiler.runtime, impl.dstLength, impl.isConst)
      nextLineNode()
    }
  }

  case class Final(compiler: Compiler, ctx: SpecializationContext, op: OperationRef) extends Node {
    protected def runInternal(): Node = null
  }

  case class SetStackSize(compiler: Compiler, ctx: SpecializationContext, op: OperationRef, stackFrameSize: Int) extends Node with UpdatesConst {
    protected def runInternal(): Node = {
      compiler.runtime.stack.setFramesize(stackFrameSize, markConst)
      nextLineNode()
    }
  }

  case class Branch(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                    impl: BranchImpl, target: OperationRef) extends Node {
    private var computedTargetNode: Node = null

    override protected def runInternal(): Node =
      if (impl.execute(compiler.runtime)) targetNode() else nextLineNode()

    def targetNode(): Node = {
      if (computedTargetNode == null) computedTargetNode = compiler.create(target, ctx)
      computedTargetNode
    }
  }

  case class Call(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                  offset: Int, target: String) extends Node {
    override protected def runInternal(): Node = {
      assert(offset >= 0)
      val prevFrame = compiler.runtime.stack.frameOffset
      compiler.runtime.stack.offset(prevFrame + offset)
      val finalCtx = callNode().run()
      compiler.runtime.stack.offset(prevFrame)
      // depending on calculation/specializations being made by callee next line node might be different
      nextLineNode(finalCtx)
    }

    private var cachedCallNode: Node = null
    private def callNode(): Node = {
      if (cachedCallNode == null) {
        val callCtx = SpecializationContext.offset(ctx, offset)
        cachedCallNode = compiler.create(OperationRef(target, 0), callCtx)
      }
      cachedCallNode
    }

    private var cachedNextLineNode = Map[SpecializationContext, Node]()
    private def nextLineNode(calleeCtx: SpecializationContext): Node =
      cachedNextLineNode.getOrElse(calleeCtx, {
        val nextLineCtx = SpecializationContext.fnCall(ctx, offset, calleeCtx)
        val node = compiler.create(OperationRef(op.fn, op.line + 1), nextLineCtx)
        cachedNextLineNode = cachedNextLineNode.updated(calleeCtx, node)
        node
      })
  }
}
