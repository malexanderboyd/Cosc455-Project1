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
  var tempPos : Int = 0
  var tempChar : Char = ' '
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
          if(!isLexeme(nextChar))
            getChar()

        }
      else if(isValidText(nextChar.toString))
        {
          // all supported characters...   The only allowed plain text in our language is: A-Z, a-z, 0-9, commas, period, quotes, colons, question marks, underscores and forward slashes.
          while(!isEOL(nextChar) && !foundTxtToken ) {
            if(isSpace(nextChar)) {
              setCurrentToken(lexeme.mkString)
              foundTxtToken = true
            }
            else {
              addChar() // addChar will handle non-lexeme supported chars.
              getChar()
            }
          }
        }
      else
        {
          if(isSpace(nextChar)) {
            if(!lexeme.contains(" ") && !isEOL(nextChar)) {
              setCurrentToken(lexeme.mkString)
              foundTxtToken = true
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


  def isSpace(c : Char) : Boolean = {
    nextChar.toString.equalsIgnoreCase(" ")
  }
  def lookup(candidateToken: String): Boolean = {
    if (Compiler.debugMode)
      println("Candidate Token: " + candidateToken)
    if (lexemes.contains(candidateToken)) {
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
      setError()
      lexeme.clear()
       false
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
   tempPos = position
   tempChar = nextChar
    getChar() // get next character
    // if next char is another\\ then the input it newline.
    if(nextChar.toString.equalsIgnoreCase(CONSTANTS.NEWLINE))
    {
        addNewLine()
    }
    else // not starting either \\use, def,etc
    {
        addLexemeType()
    }
  }

  //Deals with adding \\use \\def \\title, \begin.
  def addLexemeType() : Unit = {
    nextChar = tempChar
    position = tempPos
    lexeme += nextChar // only add one \
    while(!isEOL(nextChar) && !nextChar.toString.equalsIgnoreCase("]"))
    {
      getChar()
      if(!isLexeme(nextChar))
        addChar()
      else
        addLexemeChar()

    }
  }

  def addNewLine(): Unit =
  {
    lexeme += nextChar
    lexeme += nextChar
    lookup(lexeme.mkString)
  }
  def findClosingBrackette() : Unit = {
    lexeme += nextChar // add [ to current lexeme buffer
      while (!nextChar.toString.equals(CONSTANTS.BRACKETE) && !isEOL(nextChar)) {
        getChar()
        addChar()
      }

      if (isEOL(nextChar) && !nextChar.toString.equals(CONSTANTS.BRACKETE)) {
        setError()
        println("Line: " + Compiler.lineCount + " Syntax Error - Expected closing brackette ']' after '[' usage.")
      }
  }

  def isEOL(nc :Char): Boolean = {
      nc.toString.contains("\n")
  }


  def identifyAstr() : Unit = {
    var tempPos : Int = position
    var tempChar : Char = nextChar
    var foundTrailing : Boolean = false
    getChar() // get next character
    // if next char is another * then the input it bold.
    if(nextChar.toString.equalsIgnoreCase(CONSTANTS.ITALICS))
      {
        lexeme += nextChar
        lexeme += nextChar
        while(!foundTrailing || isEOL(nextChar)) {
          getChar()
          if (!nextChar.toString.equalsIgnoreCase(CONSTANTS.ITALICS))
            addChar()
          else {
            tempPos = position // repeat to find bold closing
            tempChar = nextChar
            getChar()
            if (nextChar.toString.equalsIgnoreCase(CONSTANTS.ITALICS)) {
              lexeme += nextChar
              lexeme += nextChar
              foundTrailing = true
            }
            else {
              println("Line : " + Compiler.lineCount + " Lexical Error: Unmatched **, expected closing ** (such as ** value ** ).")
            }
          }
        }

      }
    else // not bold, italics
      {
        nextChar = tempChar
        position = tempPos
        lexeme += nextChar // only add one *
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
    case '+' => getList()
    case '\\' => identifySlash()
    case '(' => getAddress()
    case '!' => getImage()
    case '#' => getHeading()
    case ']' => lexeme += nextChar
    case _ => println("We got a problem houston. Unknown Lexeme " + nextChar + " in addLexemeChar()")
  }
}

def  getImage() : Unit = {
  lexeme += nextChar // add '!'
  while (!isEOL(nextChar) && !nextChar.toString.equals(CONSTANTS.ADDRESSE) )
  {
    getChar()
    addChar()
  }
}
  def getList() : Unit = {
    lexeme += nextChar // add '+'
  }
  def getAddress() : Unit = {
    lexeme += nextChar // don't need to lookup since we already kinda prelooked up in isLexeme(), this is done in for all lexmes, so kinda replaces the need to look up each individual constant.
      while (!nextChar.toString.equals(CONSTANTS.ADDRESSE) && !isEOL(nextChar)) {
        getChar()
        addChar()
      }
      if (isEOL(nextChar) && !nextChar.toString.equals(CONSTANTS.BRACKETE)) {
        setError()
        println("Line: " + Compiler.lineCount + " Syntax Error - Expected closing parenthesis ')' after '(' usage.")
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

    while (!isEOL(nextChar)) // headings on their own line
    {
      getChar()
      addChar()
    }
}
  def setCurrentToken(currToken: String): Unit = {
    Compiler.currentToken_$eq(currToken)
  }
}