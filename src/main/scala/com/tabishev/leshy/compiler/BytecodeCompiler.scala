package com.tabishev.leshy.compiler

object BytecodeCompiler {
  def compile(node: Node): Node.Generated = {
    new Node.Generated(node) {}
  }
}
