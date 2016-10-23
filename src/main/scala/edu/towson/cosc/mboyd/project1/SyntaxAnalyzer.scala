package edu.towson.cosc.mboyd.project1

import scala.util.matching.Regex

class SyntaxAnalyzer extends SyntaxAnalyzerTraits {

  var errorFound: Boolean = false
  var optDef: Boolean = false
  // regex to find text yaay
  val isTextExp = "([a-zA-Z_0-9]+)".r
  // scala is c00L
  def setError() = errorFound = true

  def resetError() = errorFound = false

  def getError: Boolean = errorFound


  def gittexStart(): Boolean = {
    resetError()
    if (!errorFound) GittexBegin()
    if (!errorFound) Compiler.Scanner.getNextToken()
    if (!errorFound) EOL()
    return true;
  }

  def GittexBegin(): Unit = {
    // Check if current token is the document begin token
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      // add to parseTree
      // add here
      if (Compiler.debugMode)
        println("VALID BEGIN STATEMENT -- GittexBegin()")


    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\BEGIN at start of input when " + Compiler.currentToken + " was found.")
      setError()
    }
  }

  def gittexEnd(): Boolean = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        return true
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\END was expected at end of input " + Compiler.currentToken + " was found.")
      setError()
      return false
    }

  }

  def Title(): Boolean = {
    TitleBegin()
    if (!errorFound)
      Compiler.Scanner.getNextToken()
    if (!errorFound) TitleEnd()
    return true
  }

  def TitleBegin(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) //if(test.size == 1) // test.size will return 1 if correctly matches (should only find 1 \Title
    {
    }
    else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected either \\TITLE[ or \\DEF[ was expected after \\BEGIN. " + Compiler.currentToken + " was found.")
      setError()
    }
  }

  def TitleEnd(): Unit = {
    if (Compiler.currentToken.endsWith("]")) {
      // yay correct syntax, let main title() method call nextToken
    }
    else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\TITLE closing bracket ']' - " + Compiler.currentToken + " was found.")
      setError()
    }
  }


  override def body(): Boolean = {
    return true
  }

  override def paragraph(): Unit = ???

  override def innerText(): Unit = ???

  override def heading(): Unit = ???

  override def variableDefine(): Boolean = {
    if (Compiler.currentToken.equalsIgnoreCase("\\title[")) {
      if (Compiler.debugMode)
        println("\\TITLE[ going to variableDefine() ... aka no optional variable, moving along....")
      return false
    }
    variableDefineBegin()
    if(optDef) {
      if (!errorFound)
        Compiler.Scanner.getNextToken()
      variableName()
      if (!errorFound)
        Compiler.Scanner.getNextToken()
      if (!errorFound) variableDefineEnd()
      return true
    }

    return false
  }

  def variableName(): Unit = {
    if (isText(Compiler.currentToken)) {
        if(Compiler.currentToken.endsWith("="))
          {
              if(Compiler.debugMode)
                println("has equal sign at end. : " + Compiler.currentToken)
          }
        else {
          setError()
          println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected '=' after variable name " + Compiler.currentToken + " was found.")
        }
    }
    else {
      setError()
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected variable name " + Compiler.currentToken + " was found.")
    }
  }


  def isText(text: String): Boolean = {
    // yay lets use regex to define text cause it's fun.
    val totalMatches = isTextExp.findAllIn(text)
    if(totalMatches.size == 1) // we only should find 1 match per text token (no whitespace between)
      {
        return true
      }
    return false
  }


  def variableDefineBegin(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) //if(test.size == 1) // test.size will return 1 if correctly matches (should only find 1 \Title
    {
      if (Compiler.debugMode)
        println("Valid \\DEF[ Found.")
      optDef = true
    }
    else {
     // println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected either \\TITLE[ or \\DEF[ was expected after \\BEGIN. " + Compiler.currentToken + " was found.")
     // setError()
      // let's let Title() clean up the error it not finding a \\DEF[
      optDef = false
    }
  }

  def variableDefineEnd(): Unit = {
    if (Compiler.currentToken.endsWith("]")) {
      // yay correct syntax, let main title() method call nextToken
    }
    else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\DEF[ closing bracket ']' - " + Compiler.currentToken + " was found.")
      setError()
    }
  }


  override def variableUse(): Unit = ???

  override def bold(): Unit = ???

  override def italics(): Unit = ???

  override def listItem(): Unit = ???

  override def innerItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      Compiler.Scanner.getNextToken()
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected new line (\\n) at start of input when " + Compiler.currentToken + " was found.")
      setError()
    }
  }

  def EOL(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EOLS) || Compiler.currentToken.equalsIgnoreCase("")) {
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected new line (\\n) when " + Compiler.currentToken + " was found.")
      setError()
    }
  }


}

// end of class