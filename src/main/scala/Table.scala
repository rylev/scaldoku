package com.sudoku

class Table {
  // TODO: at some point we should define something smarter than 0 for empty.
  var board = 1 to 9 map { _ => 1 to 9 map { _ => 0 } }

  def height = board.first.length
  def width = board.length

  def setValue(x: Integer, y: Integer, value: Integer): Boolean ={
    (x, y, value) match {
      case (a, b, c) if (0 to 8 contains a) &&
                        (0 to 8 contains b) &&
                        (0 to 9 contains c) => true
      case _ => return false
    }

    board = board.indices map { col_idx =>
      board(col_idx).indices map { row_idx =>
        if (col_idx == x && row_idx == y)
          value.intValue()
        else
          board(col_idx)(row_idx)
      }
    }
    true
  }

  def getValue(x: Integer, y: Integer) : Integer = {
    board(x)(y)
  }

  def completeRow_?(rowNumber: Integer) : Boolean = {
    val rowSet = row(rowNumber).toSet
    rowSet.size == 9 && rowSet.min == 1 && rowSet.max == 9
  }

  def validRow_?(rowNumber: Integer) : Boolean = {
    val cleanRow = row(rowNumber).filter { value => value != 0 }
    cleanRow.toSet.size == cleanRow.size
  }

  def completeColumn_?(columnNumber: Integer) : Boolean = {
    val columnSet = column(columnNumber).toSet
    columnSet.size == 9 && columnSet.min == 1 && columnSet.max == 9
  }

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

  def row(number: Integer) = board.map { column => column(number) }
  def column(number: Integer) = board(number)
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