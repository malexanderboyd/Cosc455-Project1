package edu.towson.cosc.mboyd.project1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer


class LexicalAnalyzer extends LexicalAnalyzerTraits {
  var errorFound: Boolean = false
  var sourceLine: String = ""
  var tempToken : String = ""
  var lexemes = new ListBuffer[String]
  var validLexemeBuffer = new ListBuffer[String]
  var lexeme = new ArrayBuffer[Char](100)
  var nextChar: Char = ' '
  var lexLength: Int = 0
  var position: Int = 0
  var useBuffer : String = ""  // used in sending \USE[ \TITLE[ \DEF[
  var lineContainsLexemes : Boolean = false
  def setError() = errorFound = true

  def resetError() = errorFound = false

  def getError: Boolean = errorFound


  def start(line: String): Unit = {
    initializeLexems()
    sourceLine = line
    position = 0
    lexeme.clear()
    getChar()
    getNextToken()
    useBuffer = ""
    lineContainsLexemes = false
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
        println("Setting Current Token: " + tempToken)

      setCurrentToken(tempToken)
    }
  }

  def lookup(candidateToken: String): Boolean = {
    if (Compiler.debugMode)
      println("Candidate Token: " + candidateToken)
    if (lexemes.contains(candidateToken)) {
       true
    }
    else if (candidateToken.endsWith(CONSTANTS.BRACKETE) || candidateToken.endsWith(CONSTANTS.EQSIGN) || candidateToken.endsWith(CONSTANTS.ITALICS) || candidateToken.endsWith(CONSTANTS.BOLD)) {
      if (Compiler.debugMode)
        println("EndsW/Brackete: Valid Token: " + candidateToken + " found.")
       true
    }
    else if(candidateToken.startsWith(CONSTANTS.HEADING) || candidateToken.startsWith(CONSTANTS.IMAGEB))
      {
        if (Compiler.debugMode)
          println("StartsWith/#/!: Valid Token: " + candidateToken + " found.")
         true
      }
    else if(lineContainsLexemes) // we assume everything between text is valid due to checks during lexeme parsing
      {
        if (Compiler.debugMode)
          println("lineContainsLexemes: Valid Token: " + candidateToken + " found.")
        lineContainsLexemes = false
        true
      }
    else {
      Compiler.Parser.setError()
      println("Line " + Compiler.lineCount + ": LEXICAL ERROR - " + candidateToken + " is not recognized.")
      lexeme.clear()
       false
    }
  }

  def isSpace(c: Char): Boolean = {
     c == ' '
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

  def identifySlash() : Unit = {

  }

  def findClosingBrackette() : Unit = {
    lexeme += nextChar // add [ to current lexeme buffer
    while(!nextChar.toString.equals(CONSTANTS.BRACKETE) && !isEOL())
      {
        getChar()
        addChar()
      }
    if(isEOL() && !nextChar.toString.equals(CONSTANTS.BRACKETE))
      {
        setError()
        println("Line: "+ Compiler.lineCount  + " Syntax Error - Expected closing brackette ']' after '[' usage.")
      }
    if(lexeme.contains('\\')) // possibly \USE \DEF \TITLE
      {
        checkBracketteUsage()
        lineContainsLexemes = true
      }
  }
  def isEOL(): Boolean = {
    nextChar == '\n'
  }

  def identifyAstr() : Unit = {
    val tempPos : Int = position
    val tempChar : Char = nextChar
    var foundTrailing : Boolean = false
    getChar() // get next character
    // if next char is another * then the input it bold.
    if(nextChar.toString.equalsIgnoreCase(CONSTANTS.ITALICS))
      {
        lexeme += nextChar
        lexeme += nextChar
      }
    else // not bold, italics
      {
        nextChar = tempChar
        position = tempPos
        lexeme += nextChar // only add one *
        getChar()
        while(!foundTrailing || isEOL())
          {
            getChar()
            if(!nextChar.toString.equalsIgnoreCase(CONSTANTS.ITALICS))
              addChar()
            else {
              foundTrailing = true
              lexeme += nextChar
            }
          }
          if(!foundTrailing)
            {
              println("Line : " + Compiler.lineCount + " Lexical Error: Unmatched *, expected closing * (such as * value * ).")
              setError()
            }
      }
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
        case '[' => findClosingBrackette()
        case '*' => identifyAstr()
        //case '+' => getList()
        case '\\' => identifySlash()
        //case '(' => getAddress()
        //case '!' => getImage()
        case '#' => lexeme += nextChar
        case ']' => lexeme += nextChar
        case _ => println("We got a problem houston.")
      }
    }
  }

def checkBracketteUsage() : Unit = {
  // we know that the lexeme contains atleast 1 '\'. Let's try to pattern match basic \TITLE[text], \USE[varname], \DEF[varname = varvalue]
  // extract from \ to the closing ] (should contain all our information
  val starting = lexeme.indexOf('\\')
  val ending = lexeme.indexOf(']')
  val token : String = lexeme.subSequence(starting, ending+1).toString
    if(Compiler.debugMode)
    print(" LOOKUP TOKEN VALUE --- : " + token)
  if(lookup(token))
    {
      lineContainsLexemes = true
      validLexemeBuffer.append(token)
    }
  else
    {
      setError()
      println("Line: " + Compiler.lineCount + " LEXICAL ERROR: Unidentified Token: " + token)
      lineContainsLexemes = false
    }
}

def getValidLexemes(currlex : String): Unit = {
  if(validLexemeBuffer.contains("\\USE["))
    {

    }
}
  def setCurrentToken(currToken: String): Unit = {
    Compiler.currentToken_$eq(currToken)
  }
}