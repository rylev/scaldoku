package com.sudoku

abstract class Cell {
  val value: Integer
  override def toString = value.toString
}

class EmptyCell extends Cell {
  val value = null
}

class FilledCell(val value: Integer) extends Cell

class Table(board: Seq[Seq[Cell]]) {
  def this() = this(1 to 9 map { _ => 1 to 9 map { _ => new EmptyCell } })

  val height = board(0).length
  val width = board.length

  def setValue(x: Integer, y: Integer, value: Integer): Table = {
    (x, y, value) match {
      case (a, b, c) if (0 to 8 contains a) &&
                        (0 to 8 contains b) &&
                        (0 to 9 contains c) => true
      case _ => return new Table(board)
    }

    val newBoard = board.indices map { colIdx =>
      board(colIdx).indices map { rowIdx =>
        if (colIdx == x && rowIdx == y) new FilledCell(value)
        else board(colIdx)(rowIdx) } }

    new Table(newBoard)
  }

  def getValue(x: Integer, y: Integer): Integer = {
    board(x)(y).value
  }

  def completeRow_?(rowNumber: Integer) : Boolean = {
    val uniqueRow = row(rowNumber).distinct
    uniqueRow.size == 9 && uniqueRow.minBy { _.value }.value == 1 && uniqueRow.maxBy { _.value }.value == 9
  }

  def validRow_?(rowNumber: Integer) : Boolean = {
    val cleanRow = row(rowNumber).filter { cell => cell.value != 0 }
    cleanRow.distinct.size == cleanRow.size
  }

  def completeColumn_?(columnNumber: Integer) : Boolean = {
    val uniqueColumn = column(columnNumber).toSet
    uniqueColumn.size == 9 && (uniqueColumn.minBy { _.value }.value == 1) && (uniqueColumn.maxBy { _.value }.value == 9)
  }

  def validColumn_?(columnNumber: Integer) : Boolean = {
    val cleanColumn = column(columnNumber).filter { cell => cell.value != 0 }
    cleanColumn.distinct.size == cleanColumn.size
  }

  def printBoard() = {
    board.zip(board.indices).map { columnAndIndex =>
      val column = columnAndIndex._1
      column.zip(column.indices).map { numAndIndex =>
        print(numAndIndex._1)
        print(" ")
        if ((numAndIndex._2 % 3) == 2) {
          print("  ")
          if ((columnAndIndex._2 % 3) == 2 &&
               numAndIndex._2 == 7)
            println("")
        }
      }
      println("")
      if ((columnAndIndex._2 % 3) == 2)
        println("")
    }
  }

  def row(number: Integer): Seq[Cell] = board.map { column => column(number) }
  def column(number: Integer): Seq[Cell] = board(number)
  def square(number: Integer) = {
    val xOffset = (number / 3) * 3
    val yOffset = (number % 3) * 3

    0 to 2 map { a =>
      0 to 2 map { b =>
        getValue(xOffset+a, yOffset+b)
      }
    }
  }
}
