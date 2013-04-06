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

  test("validates complete rows") {
    val table = new Table()
    0 to 8 foreach { num => table.setValue(x = num, y = 0, value = num+1) }
    expect(true) { table.completeRow_?(0) }
  }

  test("validates incomplete rows") {
    val table = new Table()
    0 to 8 foreach { num => table.setValue(x = num, y = 0, value = 5) }
    expect(false) { table.completeRow_?(0) }
  }

  test("validates valid rows") {
    val table = new Table()
    0 to 8 foreach { num => table.setValue(x = num, y = 0, value = num+1) }
    table.setValue(x = 3, y = 0, value = 0)
    expect(true) { table.validRow_?(0) }
  }

  test("validates invalid rows") {
    val table = new Table()
    0 to 8 foreach { num => table.setValue(x = num, y = 0, value = 5) }
    table.setValue(x = 3, y = 0, value = 0)
    table.setValue(x = 2, y = 0, value = 4)
    expect(false) { table.validRow_?(0) }
  }
}
