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

  val boardLayout = board

  val height = board.first.length
  val width = board.length

  def setValue(x: Integer, y: Integer, value: Integer): Table ={
    (x, y, value) match {
      case (a, b, c) if (0 to 8 contains a) &&
                        (0 to 8 contains b) &&
                        (0 to 9 contains c) => true
      case _ => return new Table(board)
    }

    new Table( board.indices map { col_idx =>
      board(col_idx).indices map { row_idx =>
        if (col_idx == x && row_idx == y)
          new FilledCell(value)
        else
          board(col_idx)(row_idx)
      }
    })
  }

  def getValue(x: Integer, y: Integer): Integer = {
    boardLayout(x)(y).value
  }

  def completeRow_?(rowNumber: Integer) : Boolean = {
    val rowSet = row(rowNumber).toSet
    rowSet.size == 9 && rowSet.minBy { _.value } == 1 && rowSet.maxBy { _.value } == 9
  }

  def validRow_?(rowNumber: Integer) : Boolean = {
    val cleanRow = row(rowNumber).filter { value => value != 0 }
    cleanRow.toSet.size == cleanRow.size
  }

  def completeColumn_?(columnNumber: Integer) : Boolean = {
    val columnSet = column(columnNumber).toSet
    columnSet.size == 9 && (columnSet.minBy { _.value } == 1) && (columnSet.maxBy { _.value } == 9) }

  def validColumn_?(columnNumber: Integer) : Boolean = {
    val cleanColumn = column(columnNumber).filter { value => value != 0 }
    cleanColumn.toSet.size == cleanColumn.size
  }

  def printBoard() = {
    board.zip(board.indices).map { column_and_index =>
      val column = column_and_index._1
      column.zip(column.indices).map { num_and_index =>
        print(num_and_index._1)
        print(" ")
        if ((num_and_index._2 % 3) == 2) {
          print("  ")
          if ((column_and_index._2 % 3) == 2 &&
               num_and_index._2 == 7)
            println("")
        }
      }
      println("")
      if ((column_and_index._2 % 3) == 2)
        println("")
    }
  }

  def row(number: Integer): Seq[Cell] = boardLayout.map { column => column(number) }
  def column(number: Integer): Seq[Cell] = boardLayout(number)
  def square(number: Integer) = {
    val x_offset = (number / 3) * 3
    val y_offset = (number % 3) * 3

    0 to 2 map { a =>
      0 to 2 map { b =>
        getValue(x_offset+a, y_offset+b)
      }
    }
  }
}
