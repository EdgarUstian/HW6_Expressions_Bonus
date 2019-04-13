package expressions

import datastructures.Stack

object Expressions {

  def evaluate[A](given: String, converter: String => A, operators: Map[String,(A,A) => A], order: List[List[String]]): A = {
    //Get rid of all whitespaces
    var expression = given.replaceAll("\\s", "")
    expression = "(" + expression + ")"
//    println("No whitespace", expression)

    //Tokenize parentheses
    expression = expression.replace("(", "_(_")
    expression = expression.replace(")", "_)_")

    //Tokenize all operations
    for ((operator, function) <- operators){
      expression = expression.replace(operator, "_" + operator + "_")
    }
//    println("Tokenized", expression)

    //Split the string into Array[String] based on tokens
    val expressionList = expression.split("_").toList
//    println("List", expressionList)

    //Final ExpressionList
    var finalExp: List[String] = List()

    //Makes a new list without any empty indices
    for (value <- expressionList){
      if (value != ""){
        finalExp = finalExp :+ value
      }
    }

//    println("Final Expressions", finalExp)

    //Provides the precedence of an operation
    def findPower(operator: String): Int ={
      var index = 0
      for (i <- order.indices){
        if (order(i).contains(operator)){
          index = i
        }
      }
      index
    }

    //Make new Stack and postFixList to use
    val stack: Stack[String] = new Stack[String]
    var postFixList: List[String] = List()
    var index = 0

    //While there are token to read
    while (index < finalExp.length){ 
      val token = finalExp(index)
      //If the token is A type
      if (scala.util.Try(converter(token)).isSuccess){
        postFixList = postFixList :+ token
      }
      //If token is "("
      if (token == "("){
        stack.push(token)
      }
      //If token is ")"
      if (token == ")"){
        //While stack top is not "("
        while(stack.top.value != "("){
          postFixList = postFixList :+ stack.pop()
        }
        //If stack top is "("
        if (stack.top.value == "("){
          stack.pop()
        }
      }
      //If the token is an operator
      if (operators.contains(token)){
        //While higher or same precedence and not "("
        while(
          (stack.top != null) &&
            (stack.top.value != "(") &&
            (findPower(token) >= findPower(stack.top.value))){
          postFixList = postFixList :+ stack.pop()
        }
        stack.push(token)
      }
      index += 1
    }

    //If no more token to read
    if (index == finalExp.size){
      while(stack.top != null){
        postFixList = postFixList :+ stack.pop()
      }
    }

//    println("PostFix Notation", postFixList)

    //Assign a new list for patching
    var nextList: List[String] = postFixList

    //Reset index
    index = 0
    //PostFix expressions solver from lecture question
    while (index < nextList.size) {
      if (operators.contains(nextList(index))) {
        val operation = operators(nextList(index))
        val result: A = operation(converter(nextList(index - 2)), converter(nextList(index - 1)))
        nextList = nextList.patch(index - 2, List(result.toString), 3)
        index = 0
      }
      else {
        index += 1
      }
    }
    converter(nextList.head)
  }

  def runScript[A](filename: String, converter: String => A, operators: Map[String,(A,A) => A], order: List[List[String]]): A = {
    var variableMap: Map[String, String] = Map()
    var splitLine: List[String] = List()
    var variable: String = ""
    var expression: String = ""
    var finalString: String = ""
    val scriptFile = scala.io.Source.fromFile(filename)
    for (line <- scriptFile.getLines()){
      //Get rid of all whitespaces
      val editLine = line.replaceAll("\\s", "")
      //If lines has "=" in it split it into left and right side
      if (editLine.contains("=")){
        splitLine = editLine.split("=").toList
        variable = splitLine.head
        expression = splitLine(1)
        //Add a variable and the expression to the variable map
        if (!variableMap.contains(variable)){
          variableMap = variableMap + (variable -> expression)
        }
        //Replace the variables in expression with their original expression if already in variable map
        var index = 0
        while (index < expression.length){
          val character = expression(index).toString
          if (variableMap.contains(character)){
            expression = expression.replaceAll(character, variableMap(character))
            //Updates the replaced expression
            variableMap = variableMap + (variable -> expression)
            index = 0
          }
          else {
            //If there are no variables in expression just update variable
            variableMap = variableMap + (variable -> expression)
          }
          index += 1
        }
      }
      else {
        finalString = editLine
        //Replace all the final variables
        var index = 0
        while (index < finalString.length){
          val character = finalString(index).toString
          if (variableMap.contains(character)){
            finalString = finalString.replaceAll(character, variableMap(character))
            index = 0
          }
          else {
            index += 1
          }
        }
      }
    }
    evaluate[A]("(" + finalString + ")", converter, operators, order)
  }
}