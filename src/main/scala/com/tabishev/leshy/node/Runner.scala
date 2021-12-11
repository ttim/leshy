package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.StackMemory

object Runner {
  private val Debug: Boolean = false

  def create(ctx: RunnerCtx, node: Node): Runner = node match {
    case node: Node.Run => new CommandRunner(ctx, node)
    case node: Node.Branch => new BranchRunner(ctx, node)
    case node: Node.Call => new CallRunner(ctx, node)
    case node: Node.Final => new FinalRunner(ctx, node)
  }
}

sealed abstract class Runner {
  val ctx: RunnerCtx
  val node: Node

  def refresh(): Unit

  final def runFully(stack: StackMemory): FinalRunner = {
    var runner: Runner = this
    while (!runner.isInstanceOf[FinalRunner]) {
      debug("start run")
      val nextRunner = runner.runInternal(stack)
      debug("finish run")
      runner = nextRunner
    }

    runner.asInstanceOf[FinalRunner]
  }

  def runInternal(stack: StackMemory): Runner

  private def debug(msg: String): Unit =
    if (Runner.Debug) println(s"[${toString}]: $msg")
}

trait RunnerCtx {
  def create(node: Node): Runner
}

class CommandRunner(val ctx: RunnerCtx, val node: Node.Run) extends Runner {
  private val impl = Runners.command(node.command)
  private var next: Runner = null

  override def runInternal(stack: StackMemory): Runner = {
    impl.run(stack)
    if (next == null) next = ctx.create(node.next)
    next
  }

  override def refresh(): Unit =
    if (next != null) next = ctx.create(next.node)
}

class BranchRunner(val ctx: RunnerCtx, val node: Node.Branch) extends Runner {
  private val impl = Runners.condition(node.condition)
  private var ifTrue: Runner = null
  private var ifFalse: Runner = null

  override def runInternal(stack: StackMemory): Runner = {
    if (impl.run(stack)) {
      if (ifTrue == null) ifTrue = ctx.create(node.ifTrue)
      ifTrue
    } else {
      if (ifFalse == null) ifFalse = ctx.create(node.ifFalse)
      ifFalse
    }
  }

  override def refresh(): Unit = {
    if (ifTrue != null) ifTrue = ctx.create(ifTrue.node)
    if (ifFalse != null) ifFalse = ctx.create(ifFalse.node)
  }
}

class CallRunner(val ctx: RunnerCtx, val node: Node.Call) extends Runner {
  private val offset = node.offset.get
  private var call: Runner = null
  // todo: intern final nodes and make this map from interned id?
  private[node] var next: Map[Node.Final, Runner] = Map.empty

  override def runInternal(stack: StackMemory): Runner = {
    stack.moveFrame(offset)
    if (call == null) call = ctx.create(node.call)
    val finalRunner = call.runFully(stack)
    stack.moveFrame(-offset)
    nextRunner(finalRunner)
  }

  def nextRunner(finalRunner: FinalRunner): Runner =
    next.getOrElse(finalRunner.node, {
      next = next.updated(finalRunner.node, ctx.create(node.next(finalRunner.node)))
      next(finalRunner.node)
    })

  override def refresh(): Unit = {
    if (call != null) call = ctx.create(call.node)
    next = next.map { case (node, runner) => (node, ctx.create(runner.node)) }
  }
}

class FinalRunner(val ctx: RunnerCtx, val node: Node.Final) extends Runner {
  override def runInternal(stack: StackMemory): Runner = throw new IllegalStateException()

  override def refresh(): Unit = ()
}

abstract class GeneratedRunner extends Runner {
  override def refresh(): Unit =
    this.getClass.getDeclaredFields.foreach {
      case field if field.getType.isAssignableFrom(classOf[Runner]) =>
        val prev = field.get(this).asInstanceOf[Runner]
        field.set(this, prev.ctx.create(prev.node))
      case _ => // do nothing
    }
}

abstract class CommandImpl {
  def run(stack: StackMemory): Unit
}

abstract class ConditionImpl {
  def run(stack: StackMemory): Boolean
}
