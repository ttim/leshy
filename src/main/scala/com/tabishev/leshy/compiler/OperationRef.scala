package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.{Fn, OperationWithSource, Origin}

case class OperationRef(fn: String, line: Int) {
  def next: OperationRef = OperationRef(fn, line + 1)

  def resolve(contextFn: Fn): Option[OperationWithSource] = {
    assert(contextFn.name == fn)
    if (contextFn.ops.length == line) None else Some(contextFn.ops(line))
  }

  def origin(contextFn: Fn): Origin =
    resolve(contextFn).map(_.origin).getOrElse(Origin(contextFn.ops.head.origin.path, contextFn.ops.length))

  def toString(contextFn: Fn): String =
    s"${origin(contextFn).path.getFileName.toString}:${origin(contextFn).line}"
}
