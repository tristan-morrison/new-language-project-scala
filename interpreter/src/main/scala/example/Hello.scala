package example

import scala.collection.mutable.ArrayBuffer;

object InterpreterRunner extends App {
  val testExpr = "(+ 2 (if #f 2 3))"
  var subExprs = ArrayBuffer("")
  var currentExpr = 1

  def parse(expr: String) : Int = {
    for (char <- expr) {
      if (char.equals('(')) {
        subExprs += ""
        currentExpr = subExprs.length - 1
        subExprs(currentExpr) += char
      } else if (char.equals(')')) {
        subExprs(currentExpr) += char
        // evaluate the current expr and append it to the next-most-current expr
        subExprs(currentExpr - 1) += eval(subExprs(currentExpr))
        subExprs.remove(currentExpr)
        currentExpr -= 1
        println(subExprs(currentExpr))
      } else {
        subExprs(currentExpr) += char
      }
    }

    return 0
  }

  def eval(expr: String) : String = {
    var exprCopy: String = expr
    exprCopy = exprCopy.filterNot("()".toSet)
    var exprArr = exprCopy.split(" ")

    println(exprArr(0))

    exprArr(0) match {
      case "*" => {
        val result = exprArr(1).toInt * exprArr(2).toInt
        result.toString
      }
      case "+" => {
        val result = exprArr(1).toInt + exprArr(2).toInt
        result.toString
      }
      case "if" => {
        if (exprArr(1).equals("#t")) {
          return exprArr(2)
        } else {
          return exprArr(3)
        }
      }
    }
  }

  println(parse(testExpr))
  // println(eval("(* 2 3)"))

}
