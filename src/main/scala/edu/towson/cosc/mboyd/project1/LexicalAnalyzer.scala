package edu.towson.cosc.mboyd.project1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer


class LexicalAnalyzer extends LexicalAnalyzerTraits {

  var sourceLine: String = ""
  var lexemes = new ListBuffer[String]
  var lexeme = new ArrayBuffer[Char](100)
  var nextChar: Char = ' '
  var lexLength: Int = 0
  var position: Int = 0

  def start(line: String): Unit = {
    initializeLexems()
    sourceLine = line
    position = 0
    getChar()
    getNextToken()
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
    getNonBlank()
    addChar()
    if (!isLexeme(nextChar))
      getChar()
    // Continue gathering characters for token
    while (nextChar != CONSTANTS.EOL && !isLexeme(nextChar)) // get chars until end of line (\n)
    {
      addChar()
      if (!isLexeme(nextChar))
        getChar()
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
      case '[' => return true
      case ']' => return true
      case '!' => return true
      case '(' => return true
      case ')' => return true
      case '=' => return true
      case '*' => return true
      case _ => return false
    }
  }

  def addChar(): Unit = {
    if (lexLength <= sourceLine.length()) {
      if (!isLexeme(nextChar) && nextChar != '\n') {
        lexLength += 1
        lexeme += nextChar // appends nextChar
        getChar()
        addChar()
      }
      else {
        if (isLexeme(nextChar)) {
          // add lexeme char at end.
          lexeme += nextChar
        }
        val newToken: String = lexeme.mkString
        if (Compiler.debugMode)
          println("newtoken lookup: " + newToken)
        if (lookup(newToken)) {
          if(!newToken.equalsIgnoreCase("\n") || !newToken.equalsIgnoreCase("")) {
            setCurrentToken(newToken)
          }
          lexeme.clear()
        }
      }
    }
  }


  def setCurrentToken(currToken: String): Unit = {
    Compiler.currentToken_$eq(currToken)
  }
}