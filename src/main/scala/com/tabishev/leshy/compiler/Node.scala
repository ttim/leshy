package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{FrameOffset, Runtime}

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

// todo: add asserts on compatibility of contexts
// or maybe there is no context in node?!? and it's just part of node factory on how to create them?!
// ^ seems correct
object Node {
  final case class Options(debug: Boolean, srcOp: OperationRef, ctx: SpecializationContext)

  final case class LazyNode(options: Options, node: () => Node) extends Node {
    private var computed: Node = null

    override protected def runInternal(runtime: Runtime): Node = {
      if (computed == null) computed = node()
      computed
    }

    def computedNode(): Option[Node] = Option(computed)
  }

  final case class Run(options: Options, impl: Execution, var next: Node) extends Node {
    protected def runInternal(runtime: Runtime): Node = {
      impl.execute(runtime)
      next
    }
  }

  final case class Branch(options: Options, impl: BranchExecution, var ifTrue: Node, var ifFalse: Node) extends Node {
    override protected def runInternal(runtime: Runtime): Node =
      if (impl.execute(runtime)) ifTrue else ifFalse
  }

  final case class Call(options: Options, offset: FrameOffset, var call: Node, next: GenericNode) extends Node {
    private var cachedNextNode = Map[SpecializationContext, Node]()

    override protected def runInternal(runtime: Runtime): Node = {
      runtime.stack.moveFrame(offset.get)
      val finalCtx = call.run(runtime)
      runtime.stack.moveFrame(-offset.get)
      // depending on calculation/specializations being made by callee next line node might be different
      nextNode(finalCtx)
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

  abstract class Generated(val original: Node) extends Node {
    override val options: Options = original.options

    override protected def runInternal(runtime: Runtime): Node = original.runInternal(runtime)
  }
}
