package com.sudoku.region

import com.sudoku.cell.Cell

trait Region {
  val boardContents: Seq[Seq[Cell]]
  val index: Integer
  def cells: Seq[Cell]

  def complete_? : Boolean = {
    val uniqueCells = cells.distinct
    uniqueCells.size == 9 && (uniqueCells.minBy { _.value }.value == 1) && (uniqueCells.maxBy { _.value }.value == 9)
  }

  def valid_? : Boolean = {
    val nonEmptyCells = cells.filter { !_.empty_? }
    nonEmptyCells.distinct.size == nonEmptyCells.size
  }
}

class Square(val boardContents: Seq[Seq[Cell]], val index: Integer) extends Region {
  def cells : Seq[Cell] = {
    val rowOffset = (index / 3) * 3
    val columnOffset = (index % 3) * 3

    (0 to 2 map { a =>
      0 to 2 map { b =>
        boardContents(columnOffset+a)(rowOffset+b)
      }
    }).flatten
  }
}

class Row(val boardContents: Seq[Seq[Cell]], val index: Integer) extends Region {
  override def toString : String =
    cells.zip(cells.indices).foldLeft("") { case (output, (cell, index)) =>
      output + cell.toString + (if ((index % 3) == 2) "  " else " ")
    }

  def cells: Seq[Cell] = boardContents.map { column => column(index) }
}

class Column(val boardContents: Seq[Seq[Cell]], val index: Integer) extends Region {
  def cells: Seq[Cell] = boardContents(index)
}
