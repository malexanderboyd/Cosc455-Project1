package edu.towson.cosc.mboyd.project1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer


class LexicalAnalyzer extends LexicalAnalyzerTraits {

  var sourceLine : String = ""
  var lexemes  = new ListBuffer[String]
  var lexeme  = new  ArrayBuffer[Char](100)
  var nextChar : Char = ' '
  var lexLength : Int = 0
  var position : Int = 0
  def start(line: String) : Unit = {
    initializeLexems()
    sourceLine = line
    position = 0
    getChar()
    getNextToken()
  }
  def initializeLexems() : Unit = {
      lexemes = CONSTANTS.validLexemes
  }
  def getChar() : Unit = {
    if(position < sourceLine.length()) {
      nextChar = sourceLine.charAt(position)
      println("nextChar: " + nextChar)
      position += 1
    }
    else
      nextChar = '\n'
  }

  def getNextToken() : Unit = {
    lexLength = 0
    getNonBlank()
    addChar()
    getChar()
    // Continue gathering characters for token
    while( nextChar != CONSTANTS.EOL ) {
      addChar()
      getChar()
    }
    val newToken : String = lexeme.mkString
    println("newtoken lookup: " + newToken.substring(0, lexLength))
    if(lookup(newToken.substring(0, lexLength)))
    {
      Compiler.currentToken_$eq(newToken.substring(0, lexLength))
      lexeme.clear()
    }
  }
  def lookup(candidateToken : String) : Boolean = {
    println("Candidate Token: " + candidateToken)
    if(!lexemes.contains(candidateToken)) {
      Compiler.Parser.setError()
      println("Line " + Compiler.lineCount + ": LEXICAL ERROR -" + candidateToken + " is not recognized.")
      return true
    }
    return true
  }
  def isSpace(c : Char) : Boolean = {
    return c == ' '
  }
  def getNonBlank() : Unit = {
    while(isSpace(nextChar) && nextChar != '\n') {
      getChar()
    }
  }

  def isLexeme(nextChar : Char) : Boolean = {
    nextChar match {
      case '[' => return true
      case ']' => return true
      case '!' => return true
      case _ => return false
    }
  }
  def addChar() : Unit = {
    if(lexLength <= 98)
    {
      lexLength+=1
      lexeme += nextChar // appends nextChar
      println(lexeme)
    }
    else {
      if(!isSpace(nextChar)) {
        while(!isSpace(nextChar) && !isLexeme(nextChar)) {
          getChar()
        }
        lexLength = 0
        getNonBlank()
        addChar()
      }
    }
  }
}