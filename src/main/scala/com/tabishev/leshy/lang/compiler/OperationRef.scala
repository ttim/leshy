package com.tabishev.leshy.lang.compiler

import com.tabishev.leshy.lang.ast
import com.tabishev.leshy.lang.ast.{Fn, OperationWithSource}

case class OperationRef(fn: String, line: Int) {
  def next: OperationRef = OperationRef(fn, line + 1)

  def resolve(contextFn: Fn): Option[OperationWithSource] = {
    assert(contextFn.name == fn)
    if (contextFn.ops.length == line) None else Some(contextFn.ops(line))
  }

  def origin(contextFn: Fn): ast.Origin =
    resolve(contextFn).map(_.origin).getOrElse(ast.Origin(contextFn.ops.head.origin.path, contextFn.ops.length))

  def toString(contextFn: Fn): String =
    s"${origin(contextFn).path.getFileName.toString}:${origin(contextFn).line}"
}
