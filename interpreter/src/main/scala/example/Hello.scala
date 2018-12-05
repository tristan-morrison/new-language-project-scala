package example

import scala.collection.mutable.ArrayBuffer;

object InterpreterRunner extends App {
  val testExpr = "(+ (if #f 0 5) (* (- 5 3) 6))"
  var subExprs = ArrayBuffer("")
  var currentExpr = 1

  def interpret(expr: String) : String = {
    var desugaredInput = desugarMinus(expr)
    return parse(desugaredInput)
  }

  def cond(expr: String) : String = {
    // for convenience, get rid of the final ) in the input expr
    val input = expr.substring(0, expr.length - 1)
    val exprList = input.split('[')
    // exprList.foreach(println)

    for (currentExpr <- exprList) {
      // check to see what the first character of our test expression is
      // if it's a #, then we know we have an already-evaluated boolean primitive --- i.e., #t or #f
      // if it's a (, then we know we have an expression of form (= a b) that we need to evaluate
      if (currentExpr.charAt(1) == 't') {
        print("currentExpr: ")
        println(currentExpr)
        print("last index of space: ")
        println(currentExpr.indexOf(' ') + 1)
        print("last index of ]: ")
        println(currentExpr.lastIndexOf(']'))
        print("substring: ")
        println(currentExpr.substring(3, 4))
        return currentExpr.substring(currentExpr.indexOf(' ') + 1, currentExpr.lastIndexOf(']'))
      } else if (currentExpr.substring(0, 4) == "else") {
        return currentExpr.substring(currentExpr.indexOf(' ') + 1, currentExpr.lastIndexOf(']'))
      }
    }

    return "Returned"
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



  // println(interpret(testExpr))
  // println(cond("(cond [(= 2 3) (+ 2 3)] [#f (- 5 7)])"))
  println(parse("(+ 5 (cond [#f (+ 2 3)] [#f 22] [else 7]))"))

  def parse(expr: String) : String = {
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

    // return subExprs(0).toInt
    return subExprs(0)
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
      case "cond" => {
        println(expr)
        return cond(expr)
      }
    }
  }

  // println(parse(testExpr))
  // println(eval("(* 2 3)"))

}
