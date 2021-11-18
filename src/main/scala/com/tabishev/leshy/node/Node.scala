package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{FrameOffset, Runtime}
import org.objectweb.asm.{Label, MethodVisitor, MethodWriter}

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
    def copy(next: Node): Run

    def execute(runtime: Runtime): Unit
    def generate(writer: MethodVisitor): Unit

    final def runInternal(runtime: Runtime): Node = {
      execute(runtime)
      next
    }
  }

  abstract class Branch extends Node {
    val ifTrue: Node
    val ifFalse: Node
    def copy(ifTrue: Node, ifFalse: Node): Branch

    def execute(runtime: Runtime): Boolean
    def generate(writer: MethodVisitor, ifTrue: Label): Unit

    final override def runInternal(runtime: Runtime): Node =
      if (execute(runtime)) ifTrue else ifFalse
  }

  abstract class Call extends Node {
    val offset: FrameOffset
    val call: Node
    def copy(call: Node): Call

    def next(returnNode: Node.Final): Node
    def tryNext: Map[Node.Final, Node]

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

  abstract class Generated extends Node {
    def update(fn: Node => Node): Generated = {
      // todo: generate this method?
      val nodes = this.getClass.getDeclaredFields.collect {
        case field if (field.getType.isAssignableFrom(classOf[Node])) => {
          fn(field.get(this).asInstanceOf[Node])
        }
        case _ => // do nothing
      }
      this.getClass.getConstructors.head.newInstance(nodes :_*).asInstanceOf[Generated]
    }
  }
}
