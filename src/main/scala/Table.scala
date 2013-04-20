package com.sudoku

class Cell(val value: Integer) {
  def this() = this(null)
  override def toString = if (empty_?) "_" else value.toString
  override def equals(other: Any) : Boolean = {
    (value, other.asInstanceOf[Cell].value) match {
      case (null, null) => true
      case (null, _) => false
      case (_, null) => false
      case (a, b) => a == b
    }
  }
  override def hashCode() : Int = if (empty_?) 0 else value.hashCode
  def empty_? : Boolean = value == null
}

class Square(val boardContents: Seq[Seq[Cell]], val index: Integer) {
  def complete_? : Boolean = false
  def valid_? : Boolean = {
    val cleanSquareCells = square.filter { !_.empty_? }
    cleanSquareCells.distinct.size == cleanSquareCells.size
  }

  private def square : Seq[Cell] = {
    val xOffset = (index / 3) * 3
    val yOffset = (index % 3) * 3

    (0 to 2 map { a =>
      0 to 2 map { b =>
        boardContents(xOffset+a)(yOffset+b)
      }
    }).flatten
  }
}

class Row(val boardContents: Seq[Seq[Cell]], val index: Integer) {
  def complete_? : Boolean = {
    val uniqueRow = row(index).distinct
    uniqueRow.size == 9 && uniqueRow.minBy { _.value }.value == 1 && uniqueRow.maxBy { _.value }.value == 9
  }

  def valid_? : Boolean = {
    val cleanRowCells = row(index).filter { !_.empty_? }
    cleanRowCells.distinct.size == cleanRowCells.size
  }

  private def row(number: Integer): Seq[Cell] = boardContents.map { column => column(number) }
}

class Column(val boardContents: Seq[Seq[Cell]], val index: Integer) {
  def complete_? : Boolean = {
    val uniqueColumn = column(index).distinct
    uniqueColumn.size == 9 && (uniqueColumn.minBy { _.value }.value == 1) && (uniqueColumn.maxBy { _.value }.value == 9)
  }

  def valid_? : Boolean = {
    val cleanColumnCells = column(index).filter { !_.empty_? }
    cleanColumnCells.distinct.size == cleanColumnCells.size
  }

  private def column(number: Integer): Seq[Cell] = boardContents(number)
}

class Table(contents: Seq[Seq[Cell]]) {
  def this() = this(1 to 9 map { _ => 1 to 9 map { _ => new Cell } })

  val height = contents(0).length
  val width = contents.length
  val rows = 0 to 8 map { new Row(contents, _) }
  val columns = 0 to 8 map { new Column(contents, _) }
  val squares = 0 to 8 map { new Square(contents, _) }

  def fillCell(row: Integer, column: Integer, value: Integer): Table = {
    (row, column, value) match {
      case (row, column, value) if (0 to 8 contains row) &&
                                   (0 to 8 contains column) &&
                                   (0 to 9 contains value) => true
      case _ => return new Table(contents)
    }

    val newContents = contents.indices map { colIdx =>
      contents(colIdx).indices map { rowIdx =>
        if (colIdx == column && rowIdx == row) new Cell(value)
        else contents(colIdx)(rowIdx) } }

    new Table(newContents)
  }

  def getCell(row: Integer, column: Integer): Cell = {
    contents(column)(row)
  }

  def completeRow_?(rowNumber: Integer) : Boolean = rows(rowNumber) complete_?

  def validRow_?(rowNumber: Integer) : Boolean = rows(rowNumber) valid_?

  def completeColumn_?(columnNumber: Integer) : Boolean = columns(columnNumber) complete_?

  def validColumn_?(columnNumber: Integer) : Boolean = columns(columnNumber) valid_?

  def completeSquare_?(squareNumber: Integer) : Boolean = squares(squareNumber) complete_?

  def validSquare_?(squareNumber: Integer) : Boolean = squares(squareNumber) valid_?

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
        getCell(xOffset+a, yOffset+b)
      }
    }
  }
}
