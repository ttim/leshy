package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.StackMemory

import scala.collection.mutable

class Executor extends RunnerCtx with Stats {
  private val replaces = mutable.HashMap[Node, Node]()
  private val runners = mutable.HashMap[Node, Runner]()

  def run(node: Node, stack: StackMemory): Node.Final =
    create(node).runFully(stack).node

  def inlineCalls(predicate: Node.Call => Boolean): Unit = {
    val toInline = runners.keys.collect {
      case node: Node.Call if predicate(node) => node
    }.toArray
    toInline.foreach { node =>
      replaces.addOne(node -> Inliner.inline(node))
    }
    invalidate()
  }

  def compile(predicate: Node => Boolean): Unit = {
    val replacement = runners.collect {
      case (node, runner) if predicate(node) =>
        (node, BytecodeCompiler.compile(this, this, node)(this))
    }
    runners.addAll(replacement)
    invalidate()
  }

  private def invalidate(): Unit = {
    runners.values.foreach(_.invalidate())
  }

  override def create(node: Node): Runner = {
    val replacement = replaces.getOrElse(node, node)
    runners.getOrElseUpdate(replacement, {
      val runner = Runner.create(this, replacement)
      if (runners.size > 100) println(s"too many created nodes ${runners.size}")
      runner
    })
  }

  override def isExecuted(node: Node): Boolean = runners.contains(node)

  override def recordedCallFinals(node: Node.Call): Map[Node.Final, Node] =
    runners.get(node).map { runner =>
      runner.asInstanceOf[CallRunner].next.map { case (k, v) => (k, v.node) }
    }.getOrElse(Map())
}

// in future will expose more stats on nodes execution
trait Stats {
  def isExecuted(node: Node): Boolean
  // todo: expose only Node.Final?
  def recordedCallFinals(node: Node.Call): Map[Node.Final, Node]
}
