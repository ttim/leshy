package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.Runtime
import scala.collection.mutable

final class GenericNode(
                         val specialize: SpecializationContext => Node,
                         val specialized: mutable.Map[SpecializationContext, Node],
                       ) {
  def create(ctx: SpecializationContext): Node = specialized.getOrElseUpdate(ctx, specialize(ctx))
}

object GenericNode {
  def specialized(map: mutable.HashMap[SpecializationContext, Node]): GenericNode =
    new GenericNode(_ => throw new IllegalArgumentException, map)

  def const(node: Node): GenericNode = specialized(mutable.HashMap(node.options.ctx -> node))

  def holder(): GenericNode = specialized(mutable.HashMap())

  def of(fn: SpecializationContext => Node): GenericNode =
    new GenericNode(fn, mutable.HashMap[SpecializationContext, Node]())

  def merge(node: GenericNode, replaces: Map[SpecializationContext, Node]): GenericNode = {
    val resultNodes = mutable.HashMap[SpecializationContext, Node]()
    resultNodes.addAll(node.specialized)
    resultNodes.addAll(replaces)
    new GenericNode(node.specialize, resultNodes)
  }
}

sealed abstract class Node {
  val options: Node.Options

  // this improves perf by ~10%
  private val debug = options.debug
  private val ctx = options.ctx

  final def run(runtime: Runtime): SpecializationContext = {
//    if (maintainContext) assert(ctx == SpecializationContext.current(runtime))

    var node: Node = this
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
    if (debug) println(s"[${options.srcOp}, ${options.ctx}]: $msg")
}

object Node {
  final case class Options(debug: Boolean, srcOp: OperationRef, ctx: SpecializationContext)

  final case class Run(options: Options, impl: Execution, next: GenericNode) extends Node {
    private var computedNext: Node = null

    protected def runInternal(runtime: Runtime): Node =
      if (computedNext != null) {
        impl.execute(runtime)
        computedNext
      } else {
        impl.execute(runtime)
        val (_, prevConsts) = options.ctx.get()
        val nextContext = SpecializationContext.from(runtime.stack.frameSize(), impl.markConsts(prevConsts))
        computedNext = next.create(nextContext)
        computedNext
      }
  }

  final case class Branch(options: Options, impl: BranchExecution, ifTrue: GenericNode, ifFalse: GenericNode) extends Node {
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

  final case class Call(options: Options, offset: Int, call: GenericNode, next: GenericNode) extends Node {
    private var cachedCall: Node = null
    private var cachedNextNode = Map[SpecializationContext, Node]()

    override protected def runInternal(runtime: Runtime): Node = {
      assert(offset >= 0)
      runtime.stack.moveFrame(offset)
      val finalCtx = callNode().run(runtime)
      runtime.stack.moveFrame(-offset)
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
  }

  final case class Final(options: Options) extends Node {
    protected def runInternal(runtime: Runtime): Node = throw new IllegalStateException()
  }

  final case class RestoreCtx(options: Options, next: Node) extends Node {
    override protected def runInternal(runtime: Runtime): Node = {
      options.ctx.restore(runtime)
      next
    }
  }
}
