package edu.towson.cosc.mboyd.project1


import scala.io.Source

object Compiler {

  var currentToken: String = ""
  var lineCount = 0
  val Scanner = new LexicalAnalyzer
  val Parser = new SyntaxAnalyzer
  val Semantics = new SemanticAnalyzer
  var fileContents: String = ""
  var debugMode: Boolean = true
  var numPasses : Integer = 0
  //basic <gittex>
  var hasDocBegin: Boolean = false
  var hasOptVar: Boolean = false
  var hasTitle: Boolean = false
  var hasBody: Boolean = false
  var hasDocEnd: Boolean = false

  def main(args: Array[String]) = {
    // make sure input follows usage rules
    checkInput(args)
    Compile(args(0))
    // for each line read from file, scan and parse


  }

  def getCurrentToken() : Unit = {
    if (debugMode)
      println("Current Token going to Parser: " + currentToken)
  }

  def Compile(fileName: String): Unit = {
    for (line <- Source.fromFile(fileName).getLines()) {
      // get the first Token
      if (debugMode)
        println("Current Line being Analyzed: " + line)
      Scanner.start(line)
      // keep lineCount
      lineCount += 1

      if (!hasDocBegin && numPasses == 0) {
        if (debugMode)
          println("===== Checking for Doc Begin ====")
        getCurrentToken()
        hasDocBegin = Parser.gittexStart()
        if (debugMode)
          println("===== Status of Doc Begin:  " + hasDocBegin + " ====")
        checkForSyntaxErrors()
      }
      if (!hasOptVar && hasDocBegin && numPasses == 1) {
        if (debugMode)
          println("===== Checking for Optional Variable Def ====")
        getCurrentToken()
        hasOptVar = Parser.variableDefine()
        if (debugMode)
          println("===== Has Optional Variable:  " + hasOptVar + " ====")
        checkForSyntaxErrors()
      }
      if ( (!hasTitle && hasDocBegin && !hasOptVar && numPasses == 1) || (!hasTitle && numPasses == 2 && hasDocBegin) ) {
        if (debugMode)
          println("===== Checking for Title ====")
        getCurrentToken()
        hasTitle = Parser.Title()
        if (debugMode)
          println("===== Has Title:  " + hasTitle + " ====")
        checkForSyntaxErrors()
      }
      if (!hasBody && hasTitle && numPasses >= 2) {
        if (debugMode)
        println("===== Checking for Body Content ====")
        if(currentToken.equalsIgnoreCase(CONSTANTS.DOCE))
          {
            hasBody = true
          }
        else {
          hasBody = false
          getCurrentToken()
          Parser.body()
        }
        checkForSyntaxErrors()
      }
      numPasses += 1
    } // end of line input loop

    if (debugMode)
      println("===== Has body content: " + hasBody + " ====")
    if (!hasDocEnd && hasBody) {
      if (debugMode)
        println("===== Checking for Gittex End ====")
      hasDocEnd = Parser.gittexEnd()
      if (debugMode)
        println("===== Has Gittex End:  " + hasDocEnd + " ====")
      checkForSyntaxErrors()
    }

    Semantics.convert()

  }

  def checkForSyntaxErrors(): Unit = {
    if (Parser.getError)
      System.exit(-1)
  }

  def checkInput(inputFile: Array[String]): Unit = {
    if (inputFile.length != 1) {
      println("Usage Error: Wrong number of input arguments.")
      System.exit(0)
    }
    else if (!inputFile(0).endsWith(".mkd")) {
      println("Usage Error: Input file should be \".mkd\" extension.")
      System.exit(0)
    }

  }
}