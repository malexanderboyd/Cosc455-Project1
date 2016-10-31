package edu.towson.cosc.mboyd.project1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer


class LexicalAnalyzer extends LexicalAnalyzerTraits {

  var sourceLine: String = ""
  var tempToken : String = ""
  var lexemes = new ListBuffer[String]
  var lexeme = new ArrayBuffer[Char](100)
  var nextChar: Char = ' '
  var lexLength: Int = 0
  var position: Int = 0
  var useBuffer : String = ""  // used in sending \USE[ \TITLE[ \DEF[

  def start(line: String): Unit = {
    initializeLexems()
    sourceLine = line
    position = 0
    lexeme.clear()
    getChar()
    getNextToken()
    useBuffer = ""
  }

  def initializeLexems(): Unit = {
    lexemes = CONSTANTS.validLexemes
  }

  def getChar(): Unit = {
    if (position < sourceLine.length()) {
      nextChar = sourceLine.charAt(position)
      if (Compiler.debugMode)
        println("nextChar: " + nextChar)
      position += 1
    }
    else
      nextChar = '\n'
  }

  def getNextToken(): Unit = {
    lexLength = 0
    addChar()
    // Continue gathering characters for token
    do // get chars until end of line (\n)
    {
      getChar()
      addChar()
    }while (!isEOL())
    tempToken = lexeme.mkString
    if(lookup(tempToken))
    {
      if(useBuffer.length() > 0)
          tempToken = useBuffer + tempToken

      if(Compiler.debugMode)
        print("Setting Current Token: " + tempToken)

      setCurrentToken(tempToken)
    }
  }

  def lookup(candidateToken: String): Boolean = {
    if (Compiler.debugMode)
      println("Candidate Token: " + candidateToken)
    if (lexemes.contains(candidateToken)) {
      return true
    }
    else if (candidateToken.endsWith(CONSTANTS.BRACKETE) || candidateToken.endsWith(CONSTANTS.EQSIGN)) {
      if (Compiler.debugMode)
        println("EndsW/Brackete: Valid Token: " + candidateToken + " found.")
      return true
    }
    else if(candidateToken.startsWith(CONSTANTS.HEADING) || candidateToken.startsWith(CONSTANTS.IMAGEB))
      {
        if (Compiler.debugMode)
          println("StartsWith/#/!: Valid Token: " + candidateToken + " found.")
        return true
      }
    else {
      Compiler.Parser.setError()
      println("Line " + Compiler.lineCount + ": LEXICAL ERROR - " + candidateToken + " is not recognized.")
      lexeme.clear()
      return false
    }
  }

  def isSpace(c: Char): Boolean = {
    return c == ' '
  }

  def getNonBlank(): Unit = {
    while ((isSpace(nextChar) && nextChar != '\n') || isLexeme(nextChar)) {
      getChar()
    }
  }


  def isLexeme(nextChar: Char): Boolean = {
    nextChar match {
      case '[' =>  true
      case ']' =>  true
      case '!' =>  true
      case '(' =>  true
      case ')' =>  true
      case '*' => true
      case '#' => true
      case _ =>  false
    }
  }



  def identifyBrackette() : Unit = {
    if (lexeme.startsWith("\\")) {
      // this means that it could be either \USE[, \DEF[, \TITLE[
      lexeme.mkString match {
        case "\\DEF" => lexeme += nextChar
        case "\\USE" => lexeme += nextChar
        case "\\TITLE" => lexeme += nextChar
        case _ => println("Line: " + Compiler.lineCount + " Lexical Error: Undefined Token: " + lexeme.mkString + " expected either \\USE[, \\TITLE[, or \\DEF[")
      }
      val newToken: String = lexeme.mkString
      if (Compiler.debugMode)
        println("newtoken lookup: " + newToken)
      if (lookup(newToken)) {
         useBuffer = newToken
          lexeme.clear()
        }

    }
  }
  def isEOL(): Boolean = {
    if(nextChar == '\n')
      true
    else
      false
  }

  def identifyAstr() : Unit = {

  }
  def addChar(): Unit =
  {
    if(!isLexeme(nextChar) && !isEOL()) {
      if (lexLength <= sourceLine.length()) { // keep getting chars until a lexeme
        if (nextChar != '\n') {
          lexLength += 1
          lexeme += nextChar // appends nextChar
        }
      }
    }
    else if (isLexeme(nextChar))
    {
      // Let's do some language specific lexeme logic yay!
      nextChar match {
        case '[' => identifyBrackette()
        case '*' => identifyAstr()
        //case '+' => getList()
        //case '\\' => checkLineBreak()
        //case '(' => getAddress()
        //case '!' => getImage()
        case '#' => lexeme += nextChar
        case ']' => lexeme += nextChar
        case _ => println("We got a problem houston.")
      }
    }
  }


  def setCurrentToken(currToken: String): Unit = {
    Compiler.currentToken_$eq(currToken)
  }
}