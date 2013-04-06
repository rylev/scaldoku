package com.sudoku.test

import org.scalatest.FunSuite
import com.sudoku.Table

class TableTest extends FunSuite {

  test("width") {
    expect(9) { new Table().width }
  }

  test("height") {
    expect(9) { new Table().height }
  }

  test("setValue/getValue") {
    val table = new Table
    expect(true) { table.setValue(x = 5, y = 6, value = 5) }
    expect(5) { table.getValue(x = 5, y = 6) }
  }
}
