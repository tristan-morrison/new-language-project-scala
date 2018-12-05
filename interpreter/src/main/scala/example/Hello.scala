package example

import scala.collection.mutable.ArrayBuffer;

object InterpreterRunner extends App {
  val testExpr = "(+ (- 2 5) (- 34 (+ 2 3)))"
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
    exprCopy.filterNot("()".toSet)
    var exprArr = exprCopy.split(" ")

    
  }

  //
  // def interpreter(expr: String) : String = {
  //   var pos:Int = -1
  //   var exprCopy: String = ""
  //   var parenIndx = ArrayBuffer.empty[Int]
  //
  //   for (char <- expr) {
  //     pos += 1
  //     exprCopy += char
  //     println(exprCopy)
  //     if (char.equals('(')) {
  //       parenIndx += pos
  //     }
  //   }
  //
  //   parenIndx.foreach(println)
  //   return "Hi"
  // }

  println(parse(testExpr))

}
