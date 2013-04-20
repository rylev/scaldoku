package com.sudoku

abstract class Cell {
  val value: Integer
  override def toString = value.toString
}

class EmptyCell extends Cell {
  val value = null
}

class Row(val boardContents: Seq[Seq[Cell]], val index: Integer) {
  def complete_? : Boolean = {
    val uniqueRow = row(index).distinct
    uniqueRow.size == 9 && uniqueRow.minBy { _.value }.value == 1 && uniqueRow.maxBy { _.value }.value == 9
  }

  def valid_? : Boolean = {
    val cleanRowValues = row(index).filter { _.value != 0 }.map { _.value }
    cleanRowValues.distinct.size == cleanRowValues.size
  }

  private def row(number: Integer): Seq[Cell] = boardContents.map { column => column(number) }

  private def column(number: Integer): Seq[Cell] = boardContents(number)

}

class Column(val boardContents: Seq[Seq[Cell]], val index: Integer) {
  def complete_? : Boolean = {
    val uniqueColumn = column(index).distinct
    uniqueColumn.size == 9 && (uniqueColumn.minBy { _.value }.value == 1) && (uniqueColumn.maxBy { _.value }.value == 9)
  }

  def valid_? : Boolean = {
    val cleanColumnValues = column(index).filter { _.value != 0 }.map { _.value }
    cleanColumnValues.distinct.size == cleanColumnValues.size
  }

  private def row(number: Integer): Seq[Cell] = boardContents.map { column => column(number) }

  private def column(number: Integer): Seq[Cell] = boardContents(number)
}

class FilledCell(val value: Integer) extends Cell

class Table(contents: Seq[Seq[Cell]]) {
  def this() = this(1 to 9 map { _ => 1 to 9 map { _ => new EmptyCell } })

  val height = contents(0).length
  val width = contents.length
  val rows = 0 to 8 map { new Row(contents, _) }
  val columns = 0 to 8 map { new Column(contents, _) }

  def setValue(x: Integer, y: Integer, value: Integer): Table = {
    (x, y, value) match {
      case (a, b, c) if (0 to 8 contains a) &&
                        (0 to 8 contains b) &&
                        (0 to 9 contains c) => true
      case _ => return new Table(contents)
    }

    val newContents = contents.indices map { colIdx =>
      contents(colIdx).indices map { rowIdx =>
        if (colIdx == x && rowIdx == y) new FilledCell(value)
        else contents(colIdx)(rowIdx) } }

    new Table(newContents)
  }

  def getValue(x: Integer, y: Integer): Integer = {
    contents(x)(y).value
  }

   def completeRow_?(rowNumber: Integer) : Boolean = rows(rowNumber) complete_?

   def validRow_?(rowNumber: Integer) : Boolean = rows(rowNumber) valid_?

   def completeColumn_?(columnNumber: Integer) : Boolean = columns(columnNumber) complete_?

   def validColumn_?(columnNumber: Integer) : Boolean = columns(columnNumber) valid_?

  def printContents() = {
    contents.zip(contents.indices).map { columnAndIndex =>
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

  def row(number: Integer): Seq[Cell] = contents.map { column => column(number) }
  def column(number: Integer): Seq[Cell] = contents(number)
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
