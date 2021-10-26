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

// todo: move context out of here and make it non changeable
final case class NodeOptions(debug: Boolean, checkContext: Boolean, srcOp: OperationRef, ctx: SpecializationContext)

sealed abstract class Node {
  val options: NodeOptions

  final def run(runtime: Runtime): SpecializationContext = {
    if (options.checkContext) assert(options.ctx == SpecializationContext.current(runtime))

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
  final case class Run(options: NodeOptions, markConsts: Boolean, impl: Execution, next: NodeSupplier) extends Node {
    private var computedNext: Node = null

    protected def runInternal(runtime: Runtime): Node = {
      impl.execute(runtime)
      if (markConsts) impl.markConsts(runtime)
      nextNode(runtime)
    }

    private def nextNode(runtime: Runtime): Node = {
      if (computedNext == null)
        computedNext = next.create(SpecializationContext.current(runtime))
      computedNext
    }

    // need to restore context if not computed is a bit weird still but whatever
    def updated(options: NodeOptions = this.options, markConsts: Boolean = this.markConsts, replaces: (Node | NodeSupplier) => NodeSupplier): Node.Run = {
      val next = if (computedNext != null) replaces(computedNext) else replaces(this.next)
      Node.Run(options, markConsts, impl, next)
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

    def updated(options: NodeOptions = this.options, replaces: (Node | NodeSupplier) => NodeSupplier): Node.Branch = {
      val ifTrue = if (computedIfTrue != null) replaces(computedIfTrue) else replaces(this.ifTrue)
      val ifFalse = if (computedIfFalse != null) replaces(computedIfFalse) else replaces(this.ifFalse)
      Node.Branch(options, impl, ifTrue, ifFalse)
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
        val nextCtx = SpecializationContext.fnCall(options.ctx, offset, calleeCtx)
        val node = next.create(nextCtx)
        cachedNextNode = cachedNextNode.updated(calleeCtx, node)
        node
      })

    def updated(options: NodeOptions = this.options, replace: (Node | NodeSupplier) => NodeSupplier): Node.Call = {
      val updatedCall = if (cachedCall != null) replace(cachedCall) else replace(this.call)

      val nextCtxToSupplier = cachedNextNode.map { case (calleeCtx, node) =>
        val nextCtx = SpecializationContext.fnCall(this.options.ctx, this.offset, calleeCtx)
        (nextCtx, replace(node))
      }
      val updatedNext = new NodeSupplier {
        override def create(ctx: SpecializationContext): Node = {
          val supplier = nextCtxToSupplier.getOrElse(ctx, next)
          supplier.create(ctx)
        }
      }

      Node.Call(options, offset, updatedCall, updatedNext)
    }
  }

  final case class Final(options: NodeOptions) extends Node {
    protected def runInternal(runtime: Runtime): Node = throw new IllegalStateException()

    def updated(options: NodeOptions = this.options): Node.Final = Final(options)
  }
}
