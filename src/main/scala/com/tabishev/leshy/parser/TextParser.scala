package com.tabishev.leshy.parser

import com.tabishev.leshy.ast.{Address, Bytes, Const, Operation, Fn}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.Charset

object TextParser {
  def parse(text: String): Map[String, Fn] =
    ("\n" + text).split("\ndef ").tail.map(parseSubroutine).map { r => (r.name, r) }.toMap

  private def parseSubroutine(text: String): Fn = {
    val lines = text.split("\n")
    Fn(lines.head, lines.toSeq.tail.flatMap(parseOperation).toArray)
  }

  private def parseOperation(text: String): Option[Operation] =
    cleanOperation(text).split(" ").toSeq match {
      // stack
      case Seq("extend", length) =>
        Some(Operation.Extend(parseConst(length)))
      case Seq("shrink", length) =>
        Some(Operation.Shrink(parseConst(length)))
      case Seq("check_size", length) =>
        Some(Operation.CheckSize(parseConst(length)))
      case Seq("append", bytes) =>
        Some(Operation.Append(parseConst(bytes)))

      // branch
      case Seq("branch", modifier, lengthS, op1, op2, dest) =>
        val length = parseConst(lengthS)
        Some(Operation.Branch(parseConst(modifier), length, parseConstOrAddress(op1, Some(length)), parseConstOrAddress(op2, Some(length)), parseConst(dest)))

      // call
      case Seq("call", offset, target) =>
        Some(Operation.Call(parseConst(offset), parseConst(target)))

      // memory
      case Seq("copy", lengthS, src, dst) =>
        val length = parseConst(lengthS)
        Some(Operation.Copy(length, parseConstOrAddress(src, Some(length)), parseAddress(dst, Some(length))))

      // arithmetic
      case Seq("add", lengthS, op1, op2, dst) =>
        val length = parseConst(lengthS)
        Some(Operation.Add(length, parseConstOrAddress(op1, Some(length)), parseConstOrAddress(op2, Some(length)), parseAddress(dst, Some(length))))
      case Seq("mult", lengthS, op1, op2, dst) =>
        val length = parseConst(lengthS)
        Some(Operation.Mult(length, parseConstOrAddress(op1, Some(length)), parseConstOrAddress(op2, Some(length)), parseAddress(dst, Some(length))))
      case Seq("neg", lengthS, op, dst) =>
        val length = parseConst(lengthS)
        Some(Operation.Neg(length, parseConstOrAddress(op, Some(length)), parseAddress(dst, Some(length))))

      // "syscalls"
      case Seq("print_int", length, src) =>
        Some(Operation.PrintInt(parseConst(length), parseAddress(src, Some(parseConst(length)))))

      case Seq("") =>
        None
      case _ =>
        throw new IllegalArgumentException(s"can't parse operation '${cleanOperation(text)}'")
    }

  private def cleanOperation(operation: String): String = {
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
      // todo: remove this after I cover other cases
      Some(Bytes.fromString(arg.substring(1, arg.length - 1)))
    } else None

  private def parseAddress(arg: String, inferredLimit: Option[Const]): Address =
    if (arg.startsWith("*")) {
      Address.Native(parseConst(arg.substring(1)))
    } else if (arg.startsWith("#")) {
      Address.Stack(parseConst(arg.substring(1)), inferredLimit.get)
    } else {
      throw new IllegalArgumentException(s"can't parse address '$arg'")
    }

  private def parseConstOrAddress(arg: String, inferredLimit: Option[Const]): Const | Address =
    if (arg.startsWith("*") || arg.startsWith("#")) parseAddress(arg, inferredLimit) else parseConst(arg)
}
