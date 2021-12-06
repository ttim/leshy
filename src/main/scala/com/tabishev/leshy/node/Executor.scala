package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.StackMemory

import scala.collection.mutable

class Executor extends RunnerCtx with Stats {
  private val runners = mutable.HashMap[Node, Runner]()

  def run(node: Node, stack: StackMemory): Node.Final =
    create(node).runFully(stack).node

  def compileNodes(predicate: Node => Boolean): Unit = {
    val replacement = runners.collect {
      case (node, runner) if predicate(node) =>
        (node, BytecodeCompiler.compile(this, this, node)(this))
    }
    runners.addAll(replacement)
    runners.values.foreach(_.refresh())
  }

  override def create(node: Node): Runner = {
    runners.getOrElseUpdate(node, {
      val runner = Runner.create(this, node)
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
