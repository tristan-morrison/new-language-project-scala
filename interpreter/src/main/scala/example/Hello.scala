package example

import scala.collection.mutable.ArrayBuffer;

object InterpreterRunner extends App {
  val testExpr = "(+ (if #f 0 5) (* (- 5 3) 6))"
  var subExprs = ArrayBuffer("")
  var currentExpr = 1

  def interpret(expr: String) : Int = {
    var desugaredInput = desugarMinus(expr)
    return parse(desugaredInput)
  }

  def desugarMinus(expr: String) : String = {
    var exprCopy: String = expr

    while (exprCopy.indexOf("(-") != -1) {
      var exprFromMinus: String = exprCopy.substring(exprCopy.indexOf("(-"), (exprCopy.length()))
      var exprEndMinusPos: Int = exprFromMinus.indexOf(")")
      var exprToEval: String = exprFromMinus.substring(0, exprEndMinusPos+1)
      var newString: String = exprToEval.replace('-', '+')

      val indexOfLastSpace = newString.lastIndexOf(" ")
      val (first, second) = newString.splitAt(indexOfLastSpace + 1)
      newString = first + "-" + second
      exprCopy = exprCopy.replace(exprToEval, newString)
    }
    return exprCopy
  }



  println(interpret(testExpr))

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
        // println(subExprs(currentExpr))
      } else {
        subExprs(currentExpr) += char
      }
    }

    return subExprs(0).toInt
  }

  def eval(expr: String) : String = {
    var exprCopy: String = expr
    exprCopy = exprCopy.filterNot("()".toSet)
    var exprArr = exprCopy.split(" ")

    // println(exprArr(0))

    exprArr(0) match {
      case "*" => {
        val result = exprArr(1).toInt * exprArr(2).toInt
        result.toString
      }
      case "+" => {
        val result = exprArr(1).toInt + exprArr(2).toInt
        result.toString
      }
      case "=" => {
        if (exprArr(1) == exprArr(2)) {
          return "#t"
        } else {
          return "#f"
        }
      }
      case "if" => {
        if (exprArr(1).equals("#t")) {
          return exprArr(2).toString
        } else {
          return exprArr(3).toString
        }
      }
    }
  }

  // println(parse(testExpr))
  // println(eval("(* 2 3)"))

}
