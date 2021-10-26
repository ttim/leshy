package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Address, Bytes, Fn, OperationWithSource, Origin}
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.{CommonSymbols, Consts, MemoryRef, Runtime, StackMemory}

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

trait NodeSupplier {
  def create(ctx: SpecializationContext): Node
}

final case class NodeOptions(debug: Boolean, srcOp: OperationRef, ctx: SpecializationContext)

sealed abstract class Node {
  val options: NodeOptions

  // todo: more to NodeOptions and make it val
  private[compiler] var checkContext: Boolean = true

  final def run(runtime: Runtime): SpecializationContext = {
    if (checkContext) assert(options.ctx == SpecializationContext.current(runtime))

    var node = this
    while (!node.isInstanceOf[Node.Final]) {
      debug("start run")
      val nextNode = node.runInternal(runtime)
      debug("finish run")
      node = nextNode
    }

    node.options.ctx
  }

  protected def runInternal(runtime: Runtime): Node

  private inline def debug(inline msg: => String): Unit =
    if (options.debug) println(s"[${options.srcOp}, ${options.ctx}]: $msg")
}

object Node {
  final case class Run(options: NodeOptions, impl: Execution, next: NodeSupplier) extends Node {
    private var computedNextLine: Node = null
    // todo: make it val and arg to `Run`
    private[compiler] var markConsts: Boolean = true

    protected def runInternal(runtime: Runtime): Node = {
      impl.execute(runtime)
      if (markConsts) impl.markConsts(runtime)
      nextNode(runtime)
    }

    private def nextNode(runtime: Runtime): Node = {
      if (computedNextLine == null)
        computedNextLine = next.create(SpecializationContext.current(runtime))
      computedNextLine
    }
  }

  final case class Branch(options: NodeOptions, impl: BranchExecution, ifTrue: NodeSupplier, ifFalse: NodeSupplier) extends Node {
    private var computedIfTrue: Node = null
    private var computedIfFalse: Node = null

    override protected def runInternal(runtime: Runtime): Node =
      if (impl.execute(runtime)) ifTrueNode() else ifFalseNode()

    private def ifTrueNode(): Node = {
      if (computedIfTrue == null) computedIfTrue = ifTrue.create(options.ctx)
      computedIfTrue
    }

    private def ifFalseNode(): Node = {
      if (computedIfFalse == null) computedIfFalse = ifFalse.create(options.ctx)
      computedIfFalse
    }
  }

  final case class Call(options: NodeOptions, offset: Int, call: NodeSupplier, next: NodeSupplier) extends Node {
    private var cachedCall: Node = null
    private var cachedNextNode = Map[SpecializationContext, Node]()

    override protected def runInternal(runtime: Runtime): Node = {
      assert(offset >= 0)
      val prevFrame = runtime.stack.getFrameOffset()
      runtime.stack.offset(prevFrame + offset)
      val finalCtx = callNode().run(runtime)
      runtime.stack.offset(prevFrame)
      // depending on calculation/specializations being made by callee next line node might be different
      nextNode(finalCtx)
    }

    private def callNode(): Node = {
      if (cachedCall == null) {
        val callCtx = SpecializationContext.offset(options.ctx, offset)
        cachedCall = call.create(callCtx)
      }
      cachedCall
    }

    private def nextNode(calleeCtx: SpecializationContext): Node =
      cachedNextNode.getOrElse(calleeCtx, {
        val nextLineCtx = SpecializationContext.fnCall(options.ctx, offset, calleeCtx)
        val node = next.create(nextLineCtx)
        cachedNextNode = cachedNextNode.updated(calleeCtx, node)
        node
      })
  }

  final case class Final(options: NodeOptions) extends Node {
    protected def runInternal(runtime: Runtime): Node = throw new IllegalStateException()
  }
}
