package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.Runtime

object Compiler {
  def compile(node: Node): Node.Generated =
    new Node.Generated {
      override def runInternal(runtime: Runtime): Node = node.runInternal(runtime)
    }
}
