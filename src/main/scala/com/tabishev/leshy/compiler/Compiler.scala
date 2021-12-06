package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.loader.FnLoader
import com.tabishev.leshy.node.Executor
import com.tabishev.leshy.runtime.{FnSpec, Runtime}

object Compiler {
  def runner[T, V](
                    loader: FnLoader,
                    runtime: Runtime,
                    debugEnabled: Boolean,
                    doBytecodeGeneration: Boolean,
                    spec: FnSpec[T, V],
                    warmup: Option[T]
                  ): T => V = {
    val executor = new Executor()
    def run(input: T): V = Compiler.run(runtime, loader, executor, spec)(input)

    warmup.foreach(run)
    if (doBytecodeGeneration) executor.compileNodes {
      case node: LeshyNode => node.origin.op.line == 0
      case _ => false
    }

    input => run(input)
  }

  private def run[T, V](runtime: Runtime, loader: FnLoader, executor: Executor, spec: FnSpec[T, V])(input: T): V = {
    assert(runtime.stack.isEmpty())
    val inputObj = spec.input(input)
    runtime.stack.append(inputObj.bytes)
    val initialContext = SpecializationContext(runtime.stack.frameSize(), inputObj.consts)
    val origin = Origin(loader, runtime.symbols, OperationRef(spec.fn, 0), initialContext)
    executor.run(Nodes.create(origin), runtime)
    assert(runtime.stack.getFrameOffset() == 0)
    val output = Bytes.fromBytes(runtime.stack.currentStackFrame())
    runtime.stack.shrink(output.length())
    spec.output(output)
  }
}
