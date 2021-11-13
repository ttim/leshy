package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{FrameOffset, Runtime}

import scala.collection.mutable

sealed abstract class Node {
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

  def runInternal(runtime: Runtime): Node

  private inline def debug(inline msg: => String): Unit =
    if (Node.Debug) println(s"[${toString()}]: $msg")
}

object Node {
  private val Debug: Boolean = false

  abstract class Indirect extends Node {
    def resolve(): Node
    def tryResolve(): Option[Node]

    final override def runInternal(runtime: Runtime): Node = resolve()
  }

  abstract class Run extends Node {
    val next: Node
    def replace(withNext: Node): Run

    def execute(runtime: Runtime): Unit

    final def runInternal(runtime: Runtime): Node = {
      execute(runtime)
      next
    }
  }

  abstract class Branch extends Node {
    val ifTrue: Node
    val ifFalse: Node
    def replace(withIfTrue: Node, withIfFalse: Node): Branch

    def execute(runtime: Runtime): Boolean

    final override def runInternal(runtime: Runtime): Node =
      if (execute(runtime)) ifTrue else ifFalse
  }

  abstract class Call extends Node {
    val offset: FrameOffset
    val call: Node
    def replace(withCall: Node): Call

    def next(returnNode: Node.Final): Node

    final override def runInternal(runtime: Runtime): Node = {
      runtime.stack.moveFrame(offset.get)
      val finalNode = call.run(runtime)
      runtime.stack.moveFrame(-offset.get)
      next(finalNode)
    }
  }

  abstract class Final extends Node {
    final def runInternal(runtime: Runtime): Node = throw new IllegalStateException()
  }

  abstract class Generated extends Node
}
