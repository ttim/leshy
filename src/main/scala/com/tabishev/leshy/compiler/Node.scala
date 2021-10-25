package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Address, Bytes, Fn, OperationWithSource, Origin}
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.{CommonSymbols, Consts, MemoryRef, Runtime, StackMemory}

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class Description(op: OperationRef)

sealed abstract class Node {
  val compiler: Compiler
  val ctx: SpecializationContext
  val description: Description

  // for perf purposes
  private val debug: Boolean = compiler.debugEnabled

  final def run(runtime: Runtime): SpecializationContext = {
    debugCheck(compiler.frozen || ctx == SpecializationContext.current(compiler.runtime))

    var node = this
    while (!node.isInstanceOf[Node.Final]) {
      debug("start run")
      val nextNode = node.runInternal(runtime)
      debug("finish run")
      node = nextNode
    }

    node.ctx
  }

  protected def runInternal(runtime: Runtime): Node

  private inline def debug(inline msg: => String): Unit =
    if (debug) println(s"[$description, $ctx]: $msg")

  private inline def debugCheck(inline cond: => Boolean): Unit =
    if (debug) assert(cond)
}

object Node {
  final case class Run(compiler: Compiler, ctx: SpecializationContext, description: Description,
                       impl: Execution, next: OperationRef) extends Node {
    private var computedNextLine: Node = null

    protected def runInternal(runtime: Runtime): Node = {
      impl.execute(runtime)
      if (!compiler.frozen) impl.markConsts(runtime)
      nextNode()
    }

    private def nextNode(): Node = {
      if (computedNextLine == null)
        computedNextLine = compiler.create(next, SpecializationContext.current(compiler.runtime))
      computedNextLine
    }
  }

  final case class Branch(compiler: Compiler, ctx: SpecializationContext, description: Description,
                          impl: BranchExecution, ifTrue: OperationRef, ifFalse: OperationRef) extends Node {
    private var computedIfTrue: Node = null
    private var computedIfFalse: Node = null

    override protected def runInternal(runtime: Runtime): Node =
      if (impl.execute(runtime)) ifTrueNode() else ifFalseNode()

    private def ifTrueNode(): Node = {
      if (computedIfTrue == null) computedIfTrue = compiler.create(ifTrue, ctx)
      computedIfTrue
    }

    private def ifFalseNode(): Node = {
      if (computedIfFalse == null) computedIfFalse = compiler.create(ifFalse, ctx)
      computedIfFalse
    }
  }

  final case class Call(compiler: Compiler, ctx: SpecializationContext, description: Description,
                        offset: Int, target: String, next: OperationRef) extends Node {
    private var cachedCall: Node = null
    private var cachedNextNode = Map[SpecializationContext, Node]()

    override protected def runInternal(runtime: Runtime): Node = {
      assert(offset >= 0)
      val prevFrame = compiler.runtime.stack.getFrameOffset()
      compiler.runtime.stack.offset(prevFrame + offset)
      val finalCtx = callNode().run(runtime)
      compiler.runtime.stack.offset(prevFrame)
      // depending on calculation/specializations being made by callee next line node might be different
      nextNode(finalCtx)
    }

    private def callNode(): Node = {
      if (cachedCall == null) {
        val callCtx = SpecializationContext.offset(ctx, offset)
        cachedCall = compiler.create(OperationRef(target, 0), callCtx)
      }
      cachedCall
    }

    private def nextNode(calleeCtx: SpecializationContext): Node =
      cachedNextNode.getOrElse(calleeCtx, {
        val nextLineCtx = SpecializationContext.fnCall(ctx, offset, calleeCtx)
        val node = compiler.create(next, nextLineCtx)
        cachedNextNode = cachedNextNode.updated(calleeCtx, node)
        node
      })
  }

  final case class Final(compiler: Compiler, ctx: SpecializationContext, description: Description) extends Node {
    protected def runInternal(runtime: Runtime): Node = throw new IllegalStateException()
  }
}
