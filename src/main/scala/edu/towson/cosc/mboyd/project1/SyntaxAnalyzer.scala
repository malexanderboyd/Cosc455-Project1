package edu.towson.cosc.mboyd.project1
import scala.util.matching.Regex
class SyntaxAnalyzer extends SyntaxAnalyzerTraits {

  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError: Boolean = errorFound

  def gittex() = {
    resetError()
    if (!errorFound) GittexBegin()
    if (!errorFound) EOL()
    if (!errorFound) EOL()
    if (!errorFound) EOL()
    if (!errorFound) Title()
    if (!errorFound) EOL()
    // if (!errorFound) Body()
    if (!errorFound) GittexEnd()
    if (!errorFound) newline()
  }

  def GittexBegin(): Unit = {
    // Check if current token is the document begin token
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      // add to parseTree
      // add here

      println("VALID BEGIN STATEMENT -- GittexBegin()")
      Compiler.Scanner.getNextToken()

    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\BEGIN at start of input when " + Compiler.currentToken + " was found.")
      setError()
    }
  }

  def GittexEnd(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      Compiler.Scanner.getNextToken()
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\END was expected at end of input " + Compiler.currentToken + " was found.")
      setError()
    }

  }

  def Title() = {
    val titleRx = new Regex("\\\\TITLE\\[.+\\]")
    val test = titleRx findAllIn Compiler.currentToken
    if(test.size == 1) // test.size will return 1 if correctly matches (should only find 1 \Title
    {
      Compiler.Scanner.getNextToken()
    }
    else
      {
        println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\TITLE was expected. " + Compiler.currentToken + " was found.")
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

  override def newline(): Unit = {
    if( Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      Compiler.Scanner.getNextToken()
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected new line (\\n) at start of input when " + Compiler.currentToken + " was found.")
      setError()
    }
  }

  override def EOL() : Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EOLS)) {
      Compiler.Scanner.getNextToken()
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected new line (\\n) when " + Compiler.currentToken + " was found.")
      setError()
    }
  }




} // end of class