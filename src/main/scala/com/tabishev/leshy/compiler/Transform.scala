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
