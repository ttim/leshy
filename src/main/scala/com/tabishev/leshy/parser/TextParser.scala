package com.tabishev.leshy.parser

import com.tabishev.leshy.ast.{Address, Bytes, Const, Fn, Operation}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.Charset
import scala.collection.mutable

object TextParser {
  def parse(text: String): Map[String, Fn] =
    ("\n" + text).split("\ndef ").tail.map(parseSubroutine).map { r => (r.name, r) }.toMap

  private def parseSubroutine(text: String): Fn = {
    val ops = mutable.Buffer[Operation]()
    val labels = mutable.HashMap[String, Int]()

    val lines = text.split("\n")

    // head is name
    lines.tail.foreach { line =>
      val noComments = removeComments(line)
      if (noComments.endsWith(":")) {
        // label
        labels.put(noComments.substring(0, noComments.length - 1), ops.size)
      } else {
        // operation
        parseOperation(noComments).foreach(ops.append)
      }
    }

    Fn(lines.head, ops.toArray, labels.toMap)
  }

  private def parseOperation(text: String): Option[Operation] =
    text.split(" ").toSeq match {
      // stack
      case Seq("extend", length) =>
        Some(Operation.Extend(parseConst(length)))
      case Seq("shrink", length) =>
        Some(Operation.Shrink(parseConst(length)))
      case Seq("check_size", length) =>
        Some(Operation.CheckSize(parseConst(length)))
      case Seq("append", bytes) =>
        Some(Operation.Append(parseConst(bytes)))

      // control flow
      case Seq("branch", modifier, lengthS, op1, op2, dest) =>
        val length = parseConst(lengthS)
        Some(Operation.Branch(parseConst(modifier), length, parseConstOrAddress(op1), parseConstOrAddress(op2), parseConst(dest)))
      case Seq("jump", dest) =>
        Some(Operation.Jump(parseConst(dest)))

      // call
      case Seq("call", offset, target) =>
        Some(Operation.Call(parseConst(offset), parseConst(target)))

      // memory
      case Seq("set", lengthS, src, dst) =>
        val length = parseConst(lengthS)
        Some(Operation.Set(length, parseConstOrAddress(src), parseAddress(dst)))

      // arithmetic
      case Seq("add", lengthS, op1, op2, dst) =>
        val length = parseConst(lengthS)
        Some(Operation.Add(length, parseConstOrAddress(op1), parseConstOrAddress(op2), parseAddress(dst)))
      case Seq("mult", lengthS, op1, op2, dst) =>
        val length = parseConst(lengthS)
        Some(Operation.Mult(length, parseConstOrAddress(op1), parseConstOrAddress(op2), parseAddress(dst)))
      case Seq("neg", lengthS, op, dst) =>
        val length = parseConst(lengthS)
        Some(Operation.Neg(length, parseConstOrAddress(op), parseAddress(dst)))

      // "syscalls"
      case Seq("print_int", length, src) =>
        Some(Operation.PrintInt(parseConst(length), parseAddress(src)))

      case Seq("") =>
        None
      case _ =>
        throw new IllegalArgumentException(s"can't parse operation '${removeComments(text)}'")
    }

  private def removeComments(operation: String): String = {
    val noComment = if (operation.contains(";")) operation.substring(0, operation.indexOf(";")) else operation
    noComment.trim
  }

  private def parseConst(arg: String): Const =
    parseStackConst(arg).getOrElse(Const.Literal(parseBytes(arg)))

  private def parseBytes(arg: String): Bytes =
    parseIntBytes(arg)
      .orElse(parseConstIntWithLength(arg))
      .orElse(parseConstString(arg))
      .getOrElse(throw new IllegalArgumentException(s"can't parse bytes '$arg'"))

  private def parseIntBytes(arg: String): Option[Bytes] =
    arg.toIntOption.map(Bytes.fromInt)

  private def parseStackConst(arg: String): Option[Const] =
    if (arg.startsWith("$")) {
      Some(Const.Stack(parseBytes(arg.substring(1)), Bytes.fromInt(4)))
    } else None

  private def parseConstIntWithLength(arg: String): Option[Bytes] = {
    val es = arg.split("_")
    if (es.length == 2) {
      es(0).toLongOption.flatMap { value =>
        es(1).toIntOption.map { length =>
          assert(length <= 8)
          val buffer = Array.fill[Byte](8)(0)
          val bb = ByteBuffer.wrap(buffer)
          bb.order(ByteOrder.LITTLE_ENDIAN)
          bb.putLong(value)
          val bytes = Array.fill[Byte](length)(0)
          System.arraycopy(buffer, 0, bytes, 0, length)
          Bytes.fromBytes(bytes)
        }
      }
    } else None
  }

  private def parseConstString(arg: String): Option[Bytes] =
    if (arg.startsWith("'") && arg.endsWith("'")) {
      Some(Bytes.fromString(arg.substring(1, arg.length - 1)))
    } else if (arg.startsWith(":")) {
      Some(Bytes.fromString(arg.substring(1)))
    } else None

  private def parseAddress(arg: String): Address =
    if (arg.startsWith("*")) {
      Address.Native(parseConst(arg.substring(1)))
    } else if (arg.startsWith("#")) {
      Address.Stack(parseConst(arg.substring(1)))
    } else {
      throw new IllegalArgumentException(s"can't parse address '$arg'")
    }

  private def parseConstOrAddress(arg: String): Const | Address =
    if (arg.startsWith("*") || arg.startsWith("#")) parseAddress(arg) else parseConst(arg)
}
