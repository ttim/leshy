package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.{Bytes, Fn}

import scala.collection.mutable

object CommonSymbols {
  def register(symbols: Symbols): Unit =
    Seq("eq", "neq", "m", "le").foreach(symbols.register)
}

case class Symbol(name: String, id: Int) {
  val asBytes: Bytes = Bytes.fromInt(id)
}

class Symbols {
  private val symbolsByName: mutable.Map[String, Symbol] = mutable.HashMap()
  private val symbolsById: mutable.Map[Int, Symbol] = mutable.HashMap()
  private var nextSymbol: Int = 1

  private val loadedFns: mutable.Map[String, Fn] = mutable.HashMap()

  def register(name: String): Symbol =
    if (symbolsByName.contains(name)) symbolsByName(name) else {
      val symbol = Symbol(name, nextSymbol)
      nextSymbol += 1

      symbolsByName.put(name, symbol)
      symbolsById.put(symbol.id, symbol)

      symbol
    }

  def register(fn: Fn): Unit = loadedFns.get(fn.name) match {
    case Some(registeredFn) =>
      assert(fn == registeredFn)
    case None =>
      register(fn.name)
      fn.labels.foreach { case (name, _) => register(name) }
  }

  def resolve(name: String): Symbol = symbolsByName(name)
  def resolveById(id: Int): Symbol = symbolsById(id)
}
