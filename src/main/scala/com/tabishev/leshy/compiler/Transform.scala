package com.tabishev.leshy.compiler

import scala.annotation.tailrec
import scala.collection.mutable

trait Transform {
  def transformInScope(node: Node, replace: GenericNode => GenericNode): Node
  def transformOutOfScope(node: Node): Node
}

object Transform {
  def apply(nodes: Seq[Node], transform: Transform): Map[Node, Node] = {
    val genericNodes = mutable.ArrayBuffer[(GenericNode, GenericNode)]()

    val replacements : Map[Node, Node] = nodes.map { node =>
      node -> transform.transformInScope(node, { original =>
        val specialize = original.specialize.andThen(transform.transformOutOfScope)
        val replacement = new GenericNode(specialize, mutable.HashMap())
        genericNodes.addOne(original -> replacement)
        replacement
      })
    }.toMap

    genericNodes.foreach { case (from, to) =>
      from.specialized.foreach { case (ctx, node) =>
        val replacement = replacements.getOrElse(node, transform.transformOutOfScope(node))
        to.specialized.addOne(ctx -> replacement)
      }
    }

    replacements
  }
}

object DontMarkConsts extends Transform {
  @tailrec
  override def transformInScope(node: Node, replace: GenericNode => GenericNode): Node = {
    val optimizedOptions = node.options.copy(maintainContext = false)
    node match {
      case Node.Run(_, impl, next) =>
        Node.Run(optimizedOptions, impl, replace(next))
      case Node.Branch(_, impl, ifTrue, ifFalse) =>
        Node.Branch(optimizedOptions, impl, replace(ifTrue), replace(ifFalse))
      case Node.Call(_, offset, call, next) =>
        Node.Call(optimizedOptions, offset, replace(call), replace(next))
      case Node.Final(_) =>
        Node.Final(optimizedOptions)
      case Node.RestoreCtx(_, next) =>
        transformInScope(next, replace)
    }
  }

  override def transformOutOfScope(node: Node): Node = Node.RestoreCtx(node.options, node)
}
