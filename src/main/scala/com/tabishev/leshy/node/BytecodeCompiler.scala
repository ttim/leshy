package com.tabishev.leshy.node

import com.tabishev.leshy.bytecode.*
import com.tabishev.leshy.bytecode.BytecodeExpression.*
import com.tabishev.leshy.runtime.{Runtime, StackMemory}
import org.objectweb.asm.{ClassWriter, Label, Opcodes, Type}

import java.net.URLClassLoader
import java.nio.file.Files
import java.util.Comparator
import scala.collection.mutable
import scala.util.Random

object BytecodeCompiler {
  private val dest = new java.io.File("generated").toPath
  private val classLoader = new URLClassLoader(Array(dest.toUri.toURL), this.getClass.getClassLoader)
  private val typeGeneratedNode = Type.getType(classOf[Node.Generated])
  private val typeNode = Type.getType(classOf[Node])
  private val typeRuntime = Type.getType(classOf[Runtime])

  val RuntimeExpression: BytecodeExpression = local[Runtime](1)
  val StackExpression: BytecodeExpression = local[StackMemory](2)

  // prepare dest
  if (Files.exists(dest)) {
    Files.walk(dest)
      .sorted(Comparator.reverseOrder())
      .map(_.toFile)
      .forEach(_.delete)
  }
  Files.createDirectory(dest)

  def compile(node: Node): Node.Generated =
    BytecodeCompiler(node, "GenClass_" + Random.nextLong(Long.MaxValue)).compile()
}

private class BytecodeCompiler(node: Node, name: String) {
  import BytecodeCompiler._
  private val owner = Type.getObjectType(name)
  private val (nodes, external) = traverse(node)
  private val nodeToArg = external.zipWithIndex.map { (node, idx) => (node, argName(idx)) }.toMap

  def compile(): Node.Generated = {
    //    println(s"compile $node into $name")
    Files.write(dest.resolve(name + ".class"), generate())
    val clazz = classLoader.loadClass(name)
    val constructor = clazz.getConstructors().head
    val generated = constructor.newInstance(external.toArray:_*).asInstanceOf[Node.Generated]
    generated
  }

  def generate(): Array[Byte] = {
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)

    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, typeGeneratedNode.getInternalName, Array())
    writeFields(writer)
    writeConstructor(writer)
    writeRun(writer)
    writer.visitEnd()

    writer.toByteArray
  }

  private def writeFields(writer: ClassWriter): Unit =
    (0 until external.length).foreach { idx =>
      writer.visitField(Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL, argName(idx), typeNode.getDescriptor, null, null)
    }

  private def writeConstructor(classWriter: ClassWriter): Unit = {
    val constructorType = Type.getMethodType(Type.VOID_TYPE, Array.fill(external.length)(typeNode):_*)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "<init>", constructorType.getDescriptor, null, null)

    writer.visitCode()

    writer.statement(InvokeSuper(classOf[Node.Generated]))
    (0 until external.length).foreach { idx =>
      writer.putField(Field(isStatic = false, argName(idx), owner, typeNode), BytecodeExpression.local[Node](idx + 1))
    }

    // return
    writer.visitInsn(Opcodes.RETURN)
    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }

  private def writeRun(classWriter: ClassWriter): Unit = {
    val methodType = Type.getMethodType(typeNode, typeRuntime)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "runInternal", methodType.getDescriptor, null, null)

    val start = new Label()
    val finish = new Label()

    writer.visitCode()
    writer.visitLocalVariable("stack", Type.getDescriptor(classOf[StackMemory]), null, start, finish, 2)

    writer.visitLabel(start)
    writer.storeVar(2, BytecodeExpression.invokeVirtual(classOf[Runtime], "stack", RuntimeExpression))
    val labels = nodes.map { _ => new Label() }
    val nodeToLine = nodes.zipWithIndex.toMap

    def resolve(node: Node): Node = if (node.isInstanceOf[Node.Indirect]) {
      node.asInstanceOf[Node.Indirect].tryResolve().map(resolve).getOrElse(node)
    } else node
    def label(node: Node): Label = labels(nodeToLine(resolve(node)))
    def externalExpression(node: Node): BytecodeExpression =
      Field(isStatic = false, nodeToArg(node), owner, typeNode)
    def ret(node: Node): Unit = writer.ret(externalExpression(node))
    def jump(fromLine: Int, target: Node): Unit = {
      val resolved = resolve(target)
      if (nodes.contains(resolved)) {
        if (fromLine == nodes.length-1 || resolved != nodes(fromLine + 1)) writer.visitJumpInsn(Opcodes.GOTO, label(resolved))
      } else {
        ret(resolved)
      }
    }

    // todo: if nodes is empty?

    nodes.zipWithIndex.foreach { (node, line) =>
      // todo: don't write label per each line?
      writer.visitLabel(labels(line))

      node match {
        case run: com.tabishev.leshy.node.Node.Run =>
          run.generate(writer)
          jump(line, run.next)
        case branch: com.tabishev.leshy.node.Node.Branch =>
          branch.generate(writer, label(branch.ifTrue))
          jump(line, branch.ifFalse)
        case indirect: com.tabishev.leshy.node.Node.Indirect =>
          assert(indirect.tryResolve().isEmpty)
          jump(line, indirect)
        case call: com.tabishev.leshy.node.Node.Call =>
          // todo: inline better and make jump based on already found next nodes
          val callNode_ = Cast(externalExpression(call), classOf[Node.Call])
          val callNode = BytecodeExpression.invokeVirtual(classOf[Node.Call], "call", callNode_)

          // todo: preparing "next" arguments in advance
          writer.push(callNode_)

          writer.statement(invokeVirtual(classOf[StackMemory], "moveFrame", StackExpression, const(call.offset.get)))
          writer.push(BytecodeExpression.invokeVirtual(classOf[Node], "run", callNode, RuntimeExpression))
          writer.statement(invokeVirtual(classOf[StackMemory], "moveFrame", StackExpression, const(-call.offset.get)))

          writer.ret(BytecodeExpression.invokeVirtual(classOf[Node.Call], "next")) // callNode_ and final node args are prepared above
        case _: com.tabishev.leshy.node.Node.Final => ret(node)
        case _: com.tabishev.leshy.node.Node.Generated => ret(node)
      }
    }

    writer.visitLabel(finish)
    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }

  def traverse(root: Node): (Seq[Node], Seq[Node]) = {
    val internal = mutable.LinkedHashSet[Node]()
    val external = mutable.LinkedHashSet[Node]()

    def go(node: Node): Unit = node match {
      case _ if internal.contains(node) || external.contains(node) =>
        // do nothing
      case run: com.tabishev.leshy.node.Node.Run =>
        internal.add(run)
        go(run.next)
      case branch: com.tabishev.leshy.node.Node.Branch =>
        internal.add(branch)
        go(branch.ifFalse)
        go(branch.ifTrue)
      case indirect: com.tabishev.leshy.node.Node.Indirect =>
        indirect.tryResolve() match {
          case Some(resolved) => go(resolved)
          case None => external.add(node)
        }
      case call: com.tabishev.leshy.node.Node.Call => external.add(call)
      case _: com.tabishev.leshy.node.Node.Final => external.add(node)
      case _: com.tabishev.leshy.node.Node.Generated => external.add(node)
    }
    go(root)

    (internal.toSeq, external.toSeq)
  }

  def argName(idx: Int): String = s"node_$idx"
}
