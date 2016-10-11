package edu.towson.cosc.mboyd.project1

class SyntaxAnalyzer extends SyntaxAnalyzerTraits {

  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError: Boolean = errorFound

  def gittex() = {
    resetError()
    if (!errorFound) GittexBegin()
    if (!errorFound) Title()
    // if (!errorFound) Body()
    if (!errorFound) GittexEnd()
  }

  def GittexBegin(): Unit = {
    // Check if current token is the document begin token
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DocB)) {
      // add to parseTree
      // add hjere
      Compiler.Scanner.getNextToken()
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\BEGIN at start of input when " + Compiler.currentToken + " was found.")
      setError()
    }
  }

  def GittexEnd(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DocE)) {
      Compiler.Scanner.getNextToken()
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\END was expected at end of input " + Compiler.currentToken + " was found.")
      setError()
    }

  }

  def Title() = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLE)) {
      Compiler.Scanner.getNextToken()
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\TITLE after \\BEGIN when " + Compiler.currentToken + " was found.")
      setError()
    }
  }

  override def body(): Unit = ???

  override def paragraph(): Unit = ???

  override def innerText(): Unit = ???

  override def heading(): Unit = ???

  override def variableDefine(): Unit = ???

  override def variableUse(): Unit = ???

  override def bold(): Unit = ???

  override def italics(): Unit = ???

  override def listItem(): Unit = ???

  override def innerItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = ???
}