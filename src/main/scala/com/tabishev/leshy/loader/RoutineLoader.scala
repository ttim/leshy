package com.tabishev.leshy.loader

import com.tabishev.leshy.ast.Fn
import com.tabishev.leshy.parser.TextParser

import java.nio.file.{Files, Path}

trait RoutineLoader {
  def load(name: String): Option[Fn]
}

case class MapLoader(routines: Map[String, Fn]) extends RoutineLoader {
  override def load(name: String): Option[Fn] = routines.get(name)
}

object FileLoader {
  def fromFiles(paths: Seq[Path]): RoutineLoader =
    MapLoader(paths.flatMap(path => TextParser.parse(Files.readString(path)).toSeq).toMap)

  def fromFile(path: Path): RoutineLoader =
    MapLoader(TextParser.parse(Files.readString(path)))
}
