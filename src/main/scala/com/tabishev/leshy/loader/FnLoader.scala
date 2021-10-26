package com.tabishev.leshy.loader

import com.tabishev.leshy.ast.Fn
import com.tabishev.leshy.parser.TextParser

import java.nio.file.{Files, Path}

trait FnLoader {
  def load(name: String): Option[Fn]
}

case class MapLoader(fns: Map[String, Fn]) extends FnLoader {
  override def load(name: String): Option[Fn] = fns.get(name)
}

object FileLoader {
  def fromFiles(paths: Seq[Path]): FnLoader =
    MapLoader(paths.flatMap(path => TextParser.parse(path, Files.readString(path)).toSeq).toMap)

  def fromFile(path: Path): FnLoader =
    MapLoader(TextParser.parse(path, Files.readString(path)))
}
