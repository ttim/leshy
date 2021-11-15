package com.tabishev.leshy.node

import com.tabishev.leshy.bytecode.*
import com.tabishev.leshy.bytecode.fieldBytecodeExpression
import com.tabishev.leshy.bytecode.thisBytecodeExpression
import com.tabishev.leshy.bytecode.paramBytecodeExpression
import com.tabishev.leshy.bytecode.superBytecodeExpression
import com.tabishev.leshy.runtime.Runtime
import org.objectweb.asm.{ClassWriter, Label, Opcodes, Type}

import java.nio.file.Files
import scala.collection.mutable
import scala.util.Random

object BytecodeCompiler {
  private val classLoader = new InMemoryClassLoader(this.getClass.getClassLoader)
  private val typeGeneratedNode = Type.getType(classOf[Node.Generated])
  private val typeNode = Type.getType(classOf[Node])
  private val typeRuntime = Type.getType(classOf[Runtime])

  def compile(node: Node): Node.Generated = {
    val name = "GenClass_" + Random.nextLong(Long.MaxValue)
    val bytes = new BytecodeCompiler(node, name).compile()
    Files.write(new java.io.File("Generated.class").toPath, bytes)
    classLoader.add(name, bytes)
    val clazz = classLoader.loadClass(name)
    val constructor = clazz.getConstructors().head
    val returns = NodeTraversal.traverse(node).collect { case NodeTraversal.Statement.Return(node) => node }
    val generated = constructor.newInstance(returns.toArray:_*).asInstanceOf[Node.Generated]
    generated
  }

  private class InMemoryClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
    val classes: mutable.Map[String, Array[Byte]] = mutable.HashMap()

    def add(name: String, bytes: Array[Byte]): Unit = classes.addOne(name, bytes)

    override def findClass(name: String): Class[_] = {
      val bytes = classes(name)
      defineClass(name, bytes, 0, bytes.length)
    }
  }
}

private class BytecodeCompiler(node: Node, name: String) {
  import BytecodeCompiler._
  private val owner = Type.getObjectType(name)
  private val statements = NodeTraversal.traverse(node)
  private val args: Seq[(String, Node)] = statements
    .collect { case NodeTraversal.Statement.Return(node) => node }
    .zipWithIndex
    .map { (node, idx) => (s"node_$idx", node) }

  def compile(): Array[Byte] = {
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)

    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, typeGeneratedNode.getInternalName, Array())
    writeFields(writer)
    writeConstructor(writer)
    writeRun(writer)
    writer.visitEnd()

    writer.toByteArray
  }

  private def writeFields(writer: ClassWriter): Unit =
    (0 until args.length).foreach { idx =>
      writer.visitField(Opcodes.ACC_PRIVATE | Opcodes.ACC_FINAL, "node_" + idx, typeNode.getDescriptor, null, null)
    }

  private def writeConstructor(classWriter: ClassWriter): Unit = {
    val constructorType = Type.getMethodType(Type.VOID_TYPE, Array.fill(args.length)(typeNode):_*)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "<init>", constructorType.getDescriptor, null, null)

    writer.visitCode()

    writer.statement(InvokeSuper(classOf[Node.Generated]))
    (0 until args.length).foreach { idx =>
      writer.putField(Field(isStatic = false, "node_" + idx, owner, typeNode), Param.idx[Node](idx))
    }

    // return
    writer.visitInsn(Opcodes.RETURN)
    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }

  private def writeRun(classWriter: ClassWriter): Unit = {
    val methodType = Type.getMethodType(typeNode, typeRuntime)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "runInternal", methodType.getDescriptor, null, null)

    writer.visitCode()

    val labels = statements.collect {
      case NodeTraversal.Statement.Branch(_, target) => target
      case NodeTraversal.Statement.Jump(target) => target
    }.map { line => (line, new Label()) }.toMap

    statements.zipWithIndex.foreach { (statement, line) =>
      if (labels.contains(line)) writer.visitLabel(labels(line))

      statement match {
        case NodeTraversal.Statement.Return(node) =>
          writer.ret(Field(isStatic = false, argName(node), owner, typeNode))
        case NodeTraversal.Statement.Run(node) =>
          node.generate(writer)
        case NodeTraversal.Statement.Branch(node, ifTrue) =>
          node.generate(writer, labels(ifTrue))
        case NodeTraversal.Statement.Jump(target) =>
          writer.visitJumpInsn(Opcodes.GOTO, labels(target))
      }
    }

    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }

  private def argName(node: Node): String = args.find(_._2 == node).get._1
}

object NodeTraversal {
  enum Statement {
    case Run(node: Node.Run)
    case Branch(node: Node.Branch, ifTrue: Int)
    case Jump(statement: Int)
    case Return(node: Node)
  }

  def traverse(root: Node): Seq[Statement] = {
    val body = mutable.ArrayBuffer[Statement]()
    val generated = mutable.HashMap[Node, Int]()

    def add(statement: Statement, node: Node): Unit = {
      body.addOne(statement)
      generated.addOne((node, body.length - 1))
    }

    def ret(node: Node): Unit = add(Statement.Return(node), node)

    def go(node: Node): Unit = node match {
      case _ if generated.contains(node) =>
        body.addOne(Statement.Jump(generated(node)))
      case run: com.tabishev.leshy.node.Node.Run =>
        add(Statement.Run(run), run)
        go(run.next)
      case branch: com.tabishev.leshy.node.Node.Branch =>
        add(Statement.Branch(branch, -1), branch)
        val idx = body.length - 1
        go(branch.ifFalse)
        body(idx) = Statement.Branch(branch, body.length)
        go(branch.ifTrue)
      case indirect: com.tabishev.leshy.node.Node.Indirect =>
        indirect.tryResolve() match {
          case Some(resolved) => go(resolved)
          case None => ret(indirect)
        }
      case _: com.tabishev.leshy.node.Node.Call => ret(node)
      case _: com.tabishev.leshy.node.Node.Final => ret(node)
      case _: com.tabishev.leshy.node.Node.Generated => ret(node)
    }
    go(root)

    body.toSeq
  }
}
