package edu.towson.cosc.mboyd.project1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

class LexicalAnalyzer extends LexicalAnalyzerTraits {
  var errorFound: Boolean = false
  var sourceLine: String = ""
  var tempToken : String = ""
  var lexemes = new ListBuffer[String]
  var lexeme = new ArrayBuffer[Char](100)
  var nextChar: Char = ' '
  var lexLength: Int = 0
  var position: Int = 0
  var lineContainsLexemes : Boolean = false
  var isText : Boolean = false
  var foundTxtToken : Boolean = false
  def setError() = errorFound = true

  def resetError() = errorFound = false

  def getError: Boolean = errorFound


  def start(line: String): Unit = {
    initializeLexems()
    sourceLine = line
    position = 0
    lexeme.clear()
    getNextToken()
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
    lexeme.clear()
    lexLength = 0
    foundTxtToken = false
    getChar()
    if(isEOL(nextChar))
    {
      setCurrentToken("\\n")
    }
    while(!isEOL(nextChar) && !foundTxtToken) {
      if(isLexeme(nextChar))
        {
          addLexemeChar() // addChar will handle finding valid lexemes in tokens.
          setCurrentToken(lexeme.mkString)
          getChar()
        }
      else if(isValidText(nextChar.toString))
        {
          // all supported characters...   The only allowed plain text in our language is: A-Z, a-z, 0-9, commas, period, quotes, colons, question marks, underscores and forward slashes.
          while(!isEOL(nextChar) && !foundTxtToken ) {
            if(nextChar.toString.equalsIgnoreCase(" ")) {
              setCurrentToken(lexeme.mkString)
              foundTxtToken = true
            }
            else {
              addChar() // addChar will handle non-lexeme supported chars.
              getChar()
            }
          }
        }
      if(!lexeme.mkString.contains("\\n"))
        {
          setCurrentToken(lexeme.mkString)
          foundTxtToken = true
        }
    }
  }

  def isValidText(tmp : String) : Boolean = {
    tmp match {
      case Patterns.generalTextPattern(_) => true
      case _ => false
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
    else if(isText)
      {
        if (Compiler.debugMode)
          println("isText: Valid Token: " + candidateToken + " found.")
        isText = false
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
      case '\\' => true
      case '+' => true
      case _ =>  false
    }
  }

  def identifySlash() : Unit = {
      lexeme += nextChar
    while(!isEOL(nextChar) && !nextChar.toString.equalsIgnoreCase("]"))
      {
        getChar()
        if(!isLexeme(nextChar))
          addChar()
        else
          addLexemeChar()
      }
  }

  def findClosingBrackette() : Unit = {
    lexeme += nextChar // add [ to current lexeme buffer
    while(!nextChar.toString.equals(CONSTANTS.BRACKETE) && !isEOL(nextChar))
      {
        getChar()
        addChar()
      }
    if(isEOL(nextChar) && !nextChar.toString.equals(CONSTANTS.BRACKETE))
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
  def isEOL(nc :Char): Boolean = {
      nc.toString.contains("\n")
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
        while(!foundTrailing || isEOL(nextChar))
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

def addLexemeChar() : Unit = {
  nextChar match {
    case '[' => findClosingBrackette()
    case '*' => identifyAstr()
    //case '+' => getList()
    case '\\' => identifySlash()
    //case '(' => getAddress()
    //case '!' => getImage()
    case '#' => getHeading()
    case ']' => lexeme += nextChar
    case _ => println("We got a problem houston. Unknown Lexeme")
  }
}
  def addChar(): Unit =
  {
      if (lexLength <= sourceLine.length()) { // keep getting chars until a lexeme
        if (!isEOL(nextChar)) {
          lexLength += 1
          lexeme += nextChar // appends nextChar
        }
      }
  }

def getHeading() : Unit = {
  lexeme += nextChar // add '#'
  while(!isEOL(nextChar)) // headings on their own line
  {
    getChar()
    addChar()
  }
}

def checkBracketteUsage() : Unit = {
  // we know that the lexeme contains atleast 1 '\'. Let's try to pattern match basic \TITLE[text], \USE[varname], \DEF[varname = varvalue]
  // extract from \ to the closing ] (should contain all our information
  val starting = lexeme.indexOf('\\')
  val ending = lexeme.indexOf(']')
  val token : String = lexeme.subSequence(starting, ending+1).toString
    if(Compiler.debugMode)
    println("CHECKBRACKETTE: LOOKUP TOKEN VALUE --- : " + token)
  if(lookup(token))
    {
      lineContainsLexemes = true
    }
  else
    {
      setError()
      println("Line: " + Compiler.lineCount + " LEXICAL ERROR: Unidentified Token: " + token)
      lineContainsLexemes = false
    }
}
  def setCurrentToken(currToken: String): Unit = {
    Compiler.currentToken_$eq(currToken)
  }
}