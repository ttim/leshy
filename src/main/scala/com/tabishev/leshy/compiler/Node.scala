package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Address, Bytes, Origin}
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.{CommonSymbols, Consts, MemoryRef, Runtime, StackMemory}

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

enum MemoryOperand {
  case Stack(offset: Int)
  case Native(stackOffset: Int)

  def materialize(runtime: Runtime): MemoryRef = this match {
    case MemoryOperand.Stack(offset) => runtime.stack.getRefUnsafe(offset)
    case MemoryOperand.Native(offset) => ???
  }

  def markConst(runtime: Runtime, length: Int, isConst: Boolean) = this match {
    case MemoryOperand.Stack(offset) =>
      runtime.stack.markConst(offset, length, isConst)
    case MemoryOperand.Native(offset) =>
    // do nothing
  }
}

case class OperationRef(fn: String, line: Int)

class Compiler(val loader: RoutineLoader, val runtime: Runtime, val debugEnabled: Boolean) {
  private val constInterpreter = ConstInterpreter(runtime)
  private val registeredFns = mutable.HashSet[String]()
  private val nodes = mutable.HashMap[(OperationRef, SpecializationContext), Node]()

  private[compiler] var frozen: Boolean = false

  def freeze(): Unit = {
    frozen = true
    nodes.values.foreach {
      case node: UpdatesConst => node.markConst = false
      case _ => // do nothing
    }
  }

  private def opRepr(op: OperationRef): String = {
    val origin = operation(op).map(_.origin).getOrElse(Origin(loader.load(op.fn).get.ops.head.origin.path, -1))
    s"${origin.path.getFileName.toString}:${origin.line}"
  }

  private[compiler] inline def debug(inline op: OperationRef, inline ctx: SpecializationContext, inline msg: String, force: Boolean = false): Unit = {
    // todo: print specialization context as well?
    if (debugEnabled || force) println(s"${runtime.stack.frameToString}, ${opRepr(op)}: $msg")
  }

  private def operation(op: OperationRef): Option[ast.OperationWithSource] = {
    val ops = loader.load(op.fn).get.ops
    if (ops.length == op.line) None else Some(ops(op.line))
  }

  private def label(ctx: OperationRef, label: String): OperationRef =
    OperationRef(ctx.fn, loader.load(ctx.fn).get.labels(label))

  def run(fn: String)(init: StackMemory => Unit): Bytes = {
    assert(runtime.stack.frameOffset == 0 && runtime.stack.size == 0)
    init(runtime.stack)
    create(OperationRef(fn, 0)).run()
    assert(runtime.stack.frameOffset == 0)
    val output = Bytes.fromBytes(runtime.stack.getCurrentStackFrame())
    runtime.stack.shrink(output.length())
    output
  }

  private[compiler] def create(op: OperationRef): Node = {
    val ctx = SpecializationContext.current(runtime)
    debug(op, ctx, "start create")

    val nodeKey = (op, ctx)
    if (nodes.contains(nodeKey)) return nodes(nodeKey)

    if (frozen) throw new IllegalStateException(s"nodes are frozen, can't create node for ${opRepr(op)} with ${runtime.stack.frameToString}")

    if (!registeredFns.contains(op.fn)) {
      registeredFns.add(op.fn)
      runtime.symbols.register(op.fn)
      loader.load(op.fn).get.labels.foreach { case (label, _) => runtime.symbols.register(label) }
    }

    def simpleNode(impl: SimpleImpl): Node = Node.Simple(this, ctx, op, impl)

    val node = operation(op) match {
      case None => Node.Final(this, ctx, op)
      case Some(operation) => operation.op match {
        case ast.Operation.Extend(lengthAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          Node.SetStackSize(this, ctx, op, runtime.stack.stackFrameSize() + length)
        case ast.Operation.Shrink(lengthAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          Node.SetStackSize(this, ctx, op, runtime.stack.stackFrameSize() - length)
        case ast.Operation.Append(bytesAst) => ???
        case ast.Operation.Call(offsetAst, targetAst) =>
          val offsetRaw = constInterpreter.evalConst(offsetAst).asExpandedInt.get
          val offset = if (offsetRaw >= 0) offsetRaw else runtime.stack.stackFrameSize() + offsetRaw
          val target = constInterpreter.evalSymbol(targetAst).name
          Node.Call(this, ctx, op, offset, target)
        case ast.Operation.CheckSize(lengthAst) =>
          Node.Throw(this, ctx, op, Try {
            runtime.stack.checkSize(constInterpreter.evalConst(lengthAst).asExpandedInt.get)
          })
        case ast.Operation.Branch(modifierAst, lengthAst, op1Ast, op2Ast, targetAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val modifier = constInterpreter.evalSymbol(modifierAst).name
          val target = label(op, constInterpreter.evalSymbol(targetAst).name)

          val impl = length match {
            case 4 =>
              val op1 = toIntOrOperand(op1Ast)
              val op2 = toIntOrOperand(op2Ast)
              (modifier, op1, op2) match {
                case ("m", op1: MemoryOperand, op2: Int) => Branch.MoreMC4(op1, op2)
                case ("le", op1: MemoryOperand, op2: Int) => Branch.LessOrEqualMC4(op1, op2)
                case _ =>
                  throw new UnsupportedOperationException(modifier + " " + op1 + " " + op2)
              }
            case 8 =>
              val op1 = toLongOrOperand(op1Ast)
              val op2 = toLongOrOperand(op2Ast)
              (modifier, op1, op2) match {
                case ("m", op1: MemoryOperand, op2: Long) => Branch.MoreMC8(op1, op2)
                case _ =>
                  throw new UnsupportedOperationException(modifier + " " + op1 + " " + op2)
              }
            case _ =>
              ???
          }

          Node.Branch(this, ctx, op, impl, target)
        case ast.Operation.Jump(targetAst) =>
          val target = label(op, constInterpreter.evalSymbol(targetAst).name)
          Node.Branch(this, ctx, op, Branch.Always, target)
        case ast.Operation.PrintInt(length, src) => ???
        case ast.Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => (toIntOrOperand(op1Ast), toIntOrOperand(op2Ast)) match {
              case (v1: Int, v2: Int) => Const.Write4(v1 + v2, dst)
              case (v1: Int, v2: MemoryOperand) => Sum.MC4(v2, v1, dst)
              case (v1: MemoryOperand, v2: Int) => Sum.MC4(v1, v2, dst)
              case (v1: MemoryOperand, v2: MemoryOperand) => Sum.MM4(v1, v2, dst)
            }
            case 8 => (toLongOrOperand(op1Ast), toLongOrOperand(op2Ast)) match {
              case (v1: Long, v2: Long) => Const.Write8(v1 + v2, dst)
              case (v1: Long, v2: MemoryOperand) => Sum.MC8(v2, v1, dst)
              case (v1: MemoryOperand, v2: Long) => Sum.MC8(v1, v2, dst)
              case (v1: MemoryOperand, v2: MemoryOperand) => Sum.MM8(v1, v2, dst)
            }
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => (toIntOrOperand(op1Ast), toIntOrOperand(op2Ast)) match {
              case (v1: Int, v2: Int) => Const.Write4(v1 * v2, dst)
              case (v1: Int, v2: MemoryOperand) => Mult.MC4(v2, v1, dst)
              case (v1: MemoryOperand, v2: Int) => Mult.MC4(v1, v2, dst)
              case (v1: MemoryOperand, v2: MemoryOperand) => Mult.MM4(v1, v2, dst)
            }
            case 8 => (toLongOrOperand(op1Ast), toLongOrOperand(op2Ast)) match {
              case (v1: Long, v2: Long) => Const.Write8(v1 * v2, dst)
              case (v1: Long, v2: MemoryOperand) => Mult.MC8(v2, v1, dst)
              case (v1: MemoryOperand, v2: Long) => Mult.MC8(v1, v2, dst)
              case (v1: MemoryOperand, v2: MemoryOperand) => Mult.MM8(v1, v2, dst)
            }
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.Neg(lengthAst, opAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => toIntOrOperand(opAst) match {
              case v: Int => Const.Write4(-v, dst)
              case v: MemoryOperand => Negate.M4(v, dst)
            }
            case 8 => toLongOrOperand(opAst) match {
              case v: Long => Const.Write8(-v, dst)
              case v: MemoryOperand => Negate.M8(v, dst)
            }
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.Set(lengthAst, srcAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => toIntOrOperand(srcAst) match {
              case src: Int => Const.Write4(src, dst)
              case src: MemoryOperand => Set.M4(src, dst)
            }
            case 8 => toLongOrOperand(srcAst) match {
              case src: Long => Const.Write8(src, dst)
              case src: MemoryOperand => Set.M8(src, dst)
            }
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.NonConst(lengthAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          simpleNode(NonConst.Mark(length, toOperand(dstAst)))
        case op =>
          throw new IllegalArgumentException(s"unsupported operation '$op''")
      }
    }

    nodes.put(nodeKey, node)

    if (nodes.size > 100) debug(op, ctx, s"too many created nodes ${nodes.size}")

    debug(op, ctx, "finish create")

    node
  }

  private def toOperand(address: ast.Address): MemoryOperand = address match {
    case ast.Address.Stack(offsetAst) =>
      val rawOffset = constInterpreter.evalConst(offsetAst).asExpandedInt.get
      val offset = if (rawOffset < 0) runtime.stack.stackFrameSize() + rawOffset else rawOffset
      MemoryOperand.Stack(offset)
    case ast.Address.StackOffset(_, _, _) =>
      ???
    case ast.Address.Native(_) =>
      ???
  }

  private def toIntOrOperand(addressOrConst: ast.Address | ast.Const): Int | MemoryOperand =
    toBytesOrOperand(addressOrConst, 4, _.asInt.get)

  private def toLongOrOperand(addressOrConst: ast.Address | ast.Const): Long | MemoryOperand =
    toBytesOrOperand(addressOrConst, 8, _.asLong.get)

  private def toBytesOrOperand[T](addressOrConst: ast.Address | ast.Const, length: Int, transform: Bytes => T): T | MemoryOperand =
    addressOrConst match {
      case const: ast.Const =>
        transform(constInterpreter.evalConst(const).expand(length))
      case address: ast.Address =>
        constInterpreter.tryConst(address, length) match {
          case Some(bytes) => transform(bytes)
          case None => toOperand(address)
        }
    }
}

case class SpecializationContext private (id: Int) {
  override def toString: String = {
    val (stackSize, consts) = SpecializationContext.get(this)
    s"spec[$stackSize, $consts)]"
  }

  override def hashCode(): Int = id
  override def equals(obj: Any): Boolean = obj.isInstanceOf[SpecializationContext] &&
    obj.asInstanceOf[SpecializationContext].id == id
}

object SpecializationContext {
  private var maxId = 1
  private val contextToId = mutable.HashMap[(Int, Consts), Int]()
  private val idToContext = mutable.HashMap[Int, (Int, Consts)]()

  def from(stackSize: Int, consts: Consts): SpecializationContext = {
    val key = (stackSize, consts)
    if (contextToId.contains(key)) SpecializationContext(contextToId(key)) else {
      val newId = maxId
      maxId += 1
      contextToId.put(key, newId)
      idToContext.put(newId, key)
      SpecializationContext(newId)
    }
  }

  def get(ctx: SpecializationContext): (Int, Consts) = idToContext(ctx.id)

  def current(runtime: Runtime): SpecializationContext =
    SpecializationContext.from(runtime.stack.stackFrameSize(), runtime.stack.stackFrameConsts())
}

abstract class Node {
  private var computedNextNode: Node = null

  val compiler: Compiler
  val ctx: SpecializationContext
  val op: OperationRef

  final def checkContext(): Unit =
    if (!compiler.frozen && compiler.debugEnabled) {
      assert(ctx == SpecializationContext.current(compiler.runtime))
    }

  final def run(): SpecializationContext = {
    checkContext()

    var node = this
    while (!node.isInstanceOf[Node.Final]) {
      compiler.debug(node.op, ctx, "start run")
      val nextNode = node.runInternal()
      compiler.debug(node.op, ctx, "finish run")
      node = nextNode
    }

    node.ctx
  }

  protected def runInternal(): Node

  protected def nextLineNode(): Node = {
    if (computedNextNode == null)
      computedNextNode = compiler.create(OperationRef(op.fn, op.line + 1))
    computedNextNode
  }
}

trait UpdatesConst {
  var markConst: Boolean = true
}

object Node {
  case class Simple(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                    impl: SimpleImpl) extends Node with UpdatesConst {
    protected def runInternal(): Node = {
      impl.execute(compiler.runtime)
      if (markConst) impl.dst.markConst(compiler.runtime, impl.dstLength, impl.isConst)
      nextLineNode()
    }
  }

  case class Noop(compiler: Compiler, ctx: SpecializationContext, op: OperationRef) extends Node {
    protected def runInternal(): Node = nextLineNode()
  }

  case class Final(compiler: Compiler, ctx: SpecializationContext, op: OperationRef) extends Node {
    protected def runInternal(): Node = null
  }

  case class Throw(compiler: Compiler, ctx: SpecializationContext, op: OperationRef, ex: Try[Unit]) extends Node {
    protected def runInternal(): Node = ex match {
      case Success(_) => nextLineNode()
      case Failure(exception) => throw exception
    }
  }

  case class SetStackSize(compiler: Compiler, ctx: SpecializationContext, op: OperationRef, stackFrameSize: Int) extends Node with UpdatesConst {
    protected def runInternal(): Node = {
      compiler.runtime.stack.setFramesize(stackFrameSize, markConst)
      nextLineNode()
    }
  }

  case class Branch(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                    impl: BranchImpl, target: OperationRef) extends Node {
    private var computedTargetNode: Node = null

    override protected def runInternal(): Node =
      if (impl.execute(compiler.runtime)) targetNode() else nextLineNode()

    def targetNode(): Node = {
      if (computedTargetNode == null) computedTargetNode = compiler.create(target)
      computedTargetNode
    }
  }

  case class Call(compiler: Compiler, ctx: SpecializationContext, op: OperationRef,
                  offset: Int, target: String) extends Node {
    override protected def runInternal(): Node = {
      assert(offset >= 0)
      val prevFrame = compiler.runtime.stack.frameOffset
      compiler.runtime.stack.offset(prevFrame + offset)
      val finalCtx = callNode().run()
      compiler.runtime.stack.offset(prevFrame)
      // !!! we can't cache nextLineNode because we can't guarantee that constantness stays the same between subcalls
      // actually it's symptom, real issue is something else
      // because if everything else is correct this is not needed
      // in reality it's something around branches
      // or actually it's here, but importantly it's around main while (node != final) loop, because that's a loop which depends on actual run through branch instructions, so ctx of result node isn't guaranteed, but can be returned btw!!!
      // still weird i get result as const in fib example unless I add explicit `non_const` calls
      nextLineNode(finalCtx)
    }

    private var cachedCallNode: Node = null
    private def callNode(): Node = {
      if (cachedCallNode == null) cachedCallNode = compiler.create(OperationRef(target, 0))
      cachedCallNode
    }

    private var cachedNextLineNode = Map[SpecializationContext, Node]()
    private def nextLineNode(ctx: SpecializationContext): Node =
      cachedNextLineNode.getOrElse(ctx, {
        // todo: not true because of offsets
        //assert(SpecializationContext.current(compiler.runtime) == ctx)
        val node = compiler.create(OperationRef(op.fn, op.line + 1))
        cachedNextLineNode = cachedNextLineNode.updated(ctx, node)
        node
      })
  }
}
