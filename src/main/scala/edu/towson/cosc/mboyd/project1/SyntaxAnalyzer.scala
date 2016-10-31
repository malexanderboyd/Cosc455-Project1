package edu.towson.cosc.mboyd.project1

class SyntaxAnalyzer {

  var errorFound: Boolean = false
  var optDef: Boolean = false
  // Regex to find token patterns

  val textPattern = """([a-zA-Z_0-9]+)""".r

  // variable use
  val variableUsePattern = """(\\USE\[[a-zA-z_0-9]+\])""".r // [\USE[



  //title pattern
  val titlePattern = """(\\TITLE\[[a-zA-Z_0-9\s\']+\])""".r


  // variable define
  val variableDefPattern = """(\\DEF\[[a-zA-z_0-9]+\s?\=\s?[a-zA-z_0-9]+\])""".r   //\DEF[
  // image
  val imageStartPattern = """(\!\[)""".r // ![
  val imageTextPattern = """[a-zA-z_0-9\s]+\]""" // comment]
  val imageAddressPattern = """(\(.+\))"""// (address)

  //Links
  val linkStartPattern = """\[[a-zA-z_0-9\s]+\]""".r // [linkname]
  val linkAddressPattern = """\(.+\)""" // (linkadress)

  //New Line
  val newLinePattern = """\\\\""".r// lol nice and easy.

  // Unordered List
  val unorderedListPattern = """\+([a-zA-Z_0-9\s\']+)""".r // + listItem

  //Italics
  val italicsPattern = """(\*[a-zA-z_0-9\s]+\*)""".r// * text *

  // Bold
  val boldPattern = """(\*\*[a-zA-z_0-9\s]+\*\*)""".r// ** text **

  // Headings
  val headingPattern = """(\#[a-zA-z_0-9\s]+)""".r



  // scala is c00L
  def setError() = errorFound = true

  def resetError() = errorFound = false

  def getError: Boolean = errorFound


  def gittexStart(): Boolean = {
    resetError()
    if (!errorFound) GittexBegin()
    //if (!errorFound) Compiler.Scanner.getNextToken()
    //if (!errorFound) EOL()
    true
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
        true
    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\END was expected at end of input " + Compiler.currentToken + " was found.")
      setError()
      false
    }

  }

  def Title(): Boolean = {
    var hasTitle : Boolean = false
    Compiler.currentToken match {
      case titlePattern(_) => hasTitle = true
      case _ => setError()
    }
    hasTitle
  }

  def body(): Unit = {
      innerText()
  }

  def paragraph(): Unit = ???

 def variableUse(): Unit = {
   Compiler.currentToken match {
     case variableUsePattern(_) => println("We gonna be ight. -- TODO ADD TO STACK")
     case _ => print("Shit .")
   }
  }


   def innerText(): Unit = {
      Compiler.currentToken match {
        case variableUsePattern(_) => variableUse()
        case headingPattern(_) => heading()
        case boldPattern(_) => bold()
        case italicsPattern(_) => italics()
        case _ => println("No Match " + Compiler.currentToken)
      }
  }


   def bold(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
    println("Valid BOLD, ---- TODO ADD STACK IMPLEMENTATION")
  }

   def italics(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
    println("Valid ITALIC, ---- TODO ADD STACK IMPLEMENTATION")
  }

   def heading(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
    println("Valid Header, ---- TODO ADD STACK IMPLEMENTATION")
  }

   def variableDefine(): Boolean = {
    if (Compiler.currentToken.equalsIgnoreCase("\\title[")) {
      if (Compiler.debugMode)
        println("\\TITLE[ going to variableDefine() ... aka no optional variable, moving along....")
      return false
    }
    Compiler.currentToken match {
      case variableDefPattern(_) => true
      case _ => print(false)
              false
    }
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
    val totalMatches = textPattern.findAllIn(text)
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






   def listItem(): Unit = ???

   def innerItem(): Unit = ???

   def link(): Unit = ???

   def image(): Unit = ???

   def newline(): Unit = {
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