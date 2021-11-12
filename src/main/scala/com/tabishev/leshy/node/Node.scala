package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{FrameOffset, Runtime}

import scala.collection.mutable

sealed abstract class Node {
  val payload: Node.Payload

  final def run(runtime: Runtime): Node.Final = {
    var node: Node = this
    while (!node.isInstanceOf[Node.Final]) {
      debug("start run")
      val nextNode = node.runInternal(runtime)
      debug("finish run")
      node = nextNode
    }

    node.asInstanceOf[Node.Final]
  }

  protected def runInternal(runtime: Runtime): Node

  private inline def debug(inline msg: => String): Unit =
    if (Node.Debug) println(s"[$payload]: $msg")
}

object Node {
  private val Debug: Boolean = false

  // marker interface, can be anything
  trait Payload

  final class LazyNode(val payload: Node.Payload, val initNode: () => Node) extends Node {
    var node: Node = null

    override protected def runInternal(runtime: Runtime): Node = {
      if (node == null) node = initNode()
      node
    }
  }

  final class Run(val payload: Payload, val impl: RunImpl, var next: Node) extends Node {
    protected def runInternal(runtime: Runtime): Node = {
      impl.execute(runtime)
      next
    }
  }

  final class Branch(val payload: Payload, val impl: BranchImpl, var ifTrue: Node, var ifFalse: Node) extends Node {
    override protected def runInternal(runtime: Runtime): Node =
      if (impl.execute(runtime)) ifTrue else ifFalse
  }

  final class Call(val payload: Payload, val offset: FrameOffset, var call: Node, val initNext: Node.Final => Node) extends Node {
    var next: Map[Node.Final, Node] = Map()

    override protected def runInternal(runtime: Runtime): Node = {
      runtime.stack.moveFrame(offset.get)
      val finalNode = call.run(runtime)
      runtime.stack.moveFrame(-offset.get)
      nextNode(finalNode)
    }

    private def nextNode(calleeFinalNode: Node.Final): Node =
      next.getOrElse(calleeFinalNode, {
        val node = initNext(calleeFinalNode)
        next = next.updated(calleeFinalNode, node)
        node
      })
  }

  final class Final(val payload: Node.Payload) extends Node {
    protected def runInternal(runtime: Runtime): Node = throw new IllegalStateException()
  }

  abstract class Generated(val original: Node) extends Node {
    override val payload: Payload = original.payload
    override protected def runInternal(runtime: Runtime): Node = original.runInternal(runtime)
  }
}

abstract class RunImpl {
  def execute(runtime: Runtime): Unit
}

abstract class BranchImpl {
  def execute(runtime: Runtime): Boolean
}
