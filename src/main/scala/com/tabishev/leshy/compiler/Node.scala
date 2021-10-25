package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Address, Bytes, Fn, OperationWithSource, Origin}
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.{CommonSymbols, Consts, MemoryRef, Runtime, StackMemory}

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

sealed abstract class Node {
  val compiler: Compiler
  val ctx: SpecializationContext
  val op: OperationRef

  final def checkContext(): Unit =
    if (!compiler.frozen && compiler.debugEnabled) {
      assert(ctx == SpecializationContext.current(compiler.runtime))
    }

  final def run(runtime: Runtime): SpecializationContext = {
    checkContext()

    var node = this
    while (!node.isInstanceOf[Node.Final]) {
      compiler.debug(node.op, ctx, "start run")
      val nextNode = node.runInternal(runtime)
      compiler.debug(node.op, ctx, "finish run")
      node = nextNode
    }

    node.ctx
  }

  protected def runInternal(runtime: Runtime): Node
}

object Node {
  final case class Run(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                       impl: Execution) extends Node {
    private var computedNextLine: Node = null

    protected def runInternal(runtime: Runtime): Node = {
      impl.execute(runtime)
      if (!compiler.frozen) impl.markConsts(runtime)
      nextLineNode()
    }

    private def nextLineNode(): Node = {
      if (computedNextLine == null)
        computedNextLine = compiler.create(op.next, SpecializationContext.current(compiler.runtime))
      computedNextLine
    }
  }

  final case class Branch(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                          impl: BranchExecution, target: OperationRef) extends Node {
    private var computedNextLine: Node = null
    private var computedTarget: Node = null

    override protected def runInternal(runtime: Runtime): Node =
      if (impl.execute(runtime)) targetNode() else nextLineNode()

    private def nextLineNode(): Node = {
      if (computedNextLine == null) computedNextLine = compiler.create(op.next, ctx)
      computedNextLine
    }

    private def targetNode(): Node = {
      if (computedTarget == null) computedTarget = compiler.create(target, ctx)
      computedTarget
    }
  }

  final case class Call(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                  offset: Int, target: String) extends Node {
    private var cachedCall: Node = null
    private var cachedNextLine = Map[SpecializationContext, Node]()

    override protected def runInternal(runtime: Runtime): Node = {
      assert(offset >= 0)
      val prevFrame = compiler.runtime.stack.frameOffset
      compiler.runtime.stack.offset(prevFrame + offset)
      val finalCtx = callNode().run(runtime)
      compiler.runtime.stack.offset(prevFrame)
      // depending on calculation/specializations being made by callee next line node might be different
      nextLineNode(finalCtx)
    }

    private def callNode(): Node = {
      if (cachedCall == null) {
        val callCtx = SpecializationContext.offset(ctx, offset)
        cachedCall = compiler.create(OperationRef(target, 0), callCtx)
      }
      cachedCall
    }

    private def nextLineNode(calleeCtx: SpecializationContext): Node =
      cachedNextLine.getOrElse(calleeCtx, {
        val nextLineCtx = SpecializationContext.fnCall(ctx, offset, calleeCtx)
        val node = compiler.create(OperationRef(op.fn, op.line + 1), nextLineCtx)
        cachedNextLine = cachedNextLine.updated(calleeCtx, node)
        node
      })
  }

  final case class Final(compiler: Compiler, ctx: SpecializationContext, op: OperationRef) extends Node {
    protected def runInternal(runtime: Runtime): Node = throw new IllegalStateException()
  }
}
