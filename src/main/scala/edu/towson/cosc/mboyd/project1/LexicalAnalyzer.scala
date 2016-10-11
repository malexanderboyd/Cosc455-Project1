package edu.towson.cosc.mboyd.project1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class LexicalAnalyzer extends LexicalAnalyzerTraits {

  var sourceLine : String = ""
  var lexemes  = new ListBuffer[String]
  var lexeme  = new  ArrayBuffer[Char](100)
  var nextChar : Char = '''
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
    lexemes += "\n"
    lexemes += "\\BEGIN"
    lexemes += "\\END"
    lexemes += "\\TITLE"
    lexemes += "#"
    lexemes += "\\PARB"
    lexemes += "\\PARE"
    lexemes += "**"
    lexemes += "*"
    lexemes += "+"
    lexemes += "\\\\"
    lexemes += "["
    lexemes += "]"
    lexemes += "("
    lexemes += ")"
    lexemes += "!"
    lexemes += "\\DEF"
    lexemes += "\\USE"
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
    while(nextChar != '\n' && nextChar != ' ') {
      addChar()
      getChar()
    }
    println("Lexeme: " + lexeme.mkString)
    val newToken : String = lexeme.mkString
    println("New Token Value: " + newToken)
    if(lookup(newToken.substring(0, lexLength))) {
      Compiler.currentToken_$eq(newToken.substring(0, lexLength))
      lexeme.clear()
    }
  }
  def lookup(candidateToken : String) : Boolean = {
    println("Candidate Token: " + candidateToken)
    if(!lexemes.contains(candidateToken)) {
      Compiler.Parser.setError()
      println("Line " + Compiler.lineCount + ": LEXICAL ERROR -" + candidateToken + " is not recognized.")
      return false
    }
    return true
  }
  def isSpace(c : Char) : Boolean = {
    return c == ' '
  }
  def getNonBlank() : Unit = {
    while(isSpace(nextChar)) {
      getChar()
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
      println("Error - lexLength ... addChar() ")
      if(!isSpace(nextChar)) {
        while(!isSpace(nextChar)) {
          getChar()
        }
        lexLength = 0
        getNonBlank()
        addChar()
      }
    }
  }
}