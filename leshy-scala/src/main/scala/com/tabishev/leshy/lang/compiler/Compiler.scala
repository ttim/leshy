package com.tabishev.leshy.lang.compiler

import com.tabishev.leshy.runtime.{Bytes, StackMemory}
import com.tabishev.leshy.lang.common.{FnSpec, Symbols}
import com.tabishev.leshy.lang.loader.FnLoader
import com.tabishev.leshy.node.Executor

object Compiler {
  def runner[T, V](
                    loader: FnLoader,
                    stack: StackMemory,
                    symbols: Symbols,
                    debugEnabled: Boolean,
                    doInlining: Boolean,
                    doBytecodeGeneration: Boolean,
                    spec: FnSpec[T, V],
                    warmup: Option[T]
                  ): T => V = {
    val executor = new Executor()
    def run(input: T): V = Compiler.run(stack, symbols, loader, executor, spec)(input)

    warmup.foreach(run)
    if (doInlining) {
      executor.inlineCalls(_ => true)
      warmup.foreach(run)
    }
    if (doBytecodeGeneration) {
      executor.compile {
        case node: LeshyNode => node.op.line == 0
        case _ => false
      }
    }

    input => run(input)
  }

  private def run[T, V](stack: StackMemory, symbols: Symbols, loader: FnLoader, executor: Executor, spec: FnSpec[T, V])(input: T): V = {
    assert(stack.isEmpty())
    val inputObj = spec.input(input)
    stack.append(inputObj.bytes)
    val initialContext = SpecializationContext(stack.frameSize(), inputObj.consts)
    executor.run(LeshyNode(loader, symbols, OperationRef(spec.fn, 0), initialContext), stack)
    assert(stack.getFrameOffset() == 0)
    val output = Bytes.fromBytes(stack.currentStackFrame())
    stack.shrink(output.length())
    spec.output(output)
  }
}
