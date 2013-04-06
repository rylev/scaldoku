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

  test("validates complete columns") {
    val table = new Table()
    0 to 8 foreach { num => table.setValue(x = 0, y = num, value = num+1) }
    expect(true) { table.completeColumn_?(0) }
  }

  test("validates incomplete columns") {
    val table = new Table()
    0 to 8 foreach { num => table.setValue(x = 0, y = num, value = 5) }
    expect(false) { table.completeColumn_?(0) }
  }

  test("validates valid columns") {
    val table = new Table()
    0 to 8 foreach { num => table.setValue(x = 0, y = num, value = num+1) }
    table.setValue(y = 3, x = 0, value = 0)
    expect(true) { table.validColumn_?(0) }
  }

  test("validates invalid columns") {
    val table = new Table()
    0 to 8 foreach { num => table.setValue(x = 0, y = num, value = 5) }
    table.setValue(y = 3, x = 0, value = 0)
    table.setValue(y = 2, x = 0, value = 4)
    expect(false) { table.validColumn_?(0) }
  }

  test("gets a square") {
    val table = new Table()
    0 to 8 foreach { a =>
      0 to 8 foreach { b =>
        val value = if ((a+b) % 2 == 0)
          a
        else
          b
        table.setValue(x = a, y = b, value = value)
      }
    }

    table.printBoard()

    val third = table.square(3)
    val fourth = table.square(4)
    val thirdShouldEqual = Vector(Vector(0,3,2), Vector(4,1,4), Vector(0,5,2))
    val fourthShouldEqual = Vector(Vector(3,4,3), Vector(3,4,5), Vector(5,4,5))

    expect(thirdShouldEqual) { third }
    expect(fourthShouldEqual) { fourth }


//
//
//    table.setValue(y = 3, x = 0, value = 0)
//    table.setValue(y = 2, x = 0, value = 4)
//    expect(false) { table.validColumn_?(0) }
  }
}
