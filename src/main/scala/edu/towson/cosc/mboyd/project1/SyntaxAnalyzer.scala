package edu.towson.cosc.mboyd.project1

class SyntaxAnalyzer {

  var errorFound: Boolean = false
  var optDef: Boolean = false
  var parabcounter : Integer = 0
  var pareEcounter : Integer = 0 // lol pls dont have 9999 parab and parae. Terrible way to implement this to check if para end and start, but these should be equal if correct.
  // scala is c00L
  def setError() = errorFound = true

  def resetError() = errorFound = false

  def getError: Boolean = errorFound


  def gittexStart(): Boolean = {
    resetError()
    if (!errorFound) GittexBegin()
    true
  }

  def GittexBegin(): Unit = {
    // Check if current token is the document begin token
    println("CURRENT TOKEN: " + Compiler.currentToken)
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
      case Patterns.titlePattern(_) => hasTitle = true
      case _ => setError()
    }
    hasTitle
  }
var numBodyPasses : Integer = 0
  def body(): Unit = {
      variableDefine()
      innerText()
      //paragraph()
      Compiler.Scanner.getNextToken()
      while(!Compiler.currentToken.equalsIgnoreCase("\\n"))
        {
          body()
        }
  }

  def paragraph(): Unit = {
    parabcounter += 1
    if(Compiler.debugMode)
      println("VALID PARAB FOUND -- ADD STACK")
  }

  def paragraphEnd() : Unit = {
    pareEcounter += 1
    if(Compiler.debugMode)
      println("VALID PARE FOUND -- ADD STACK")
  }

 def variableUse(): Unit = {
   Compiler.currentToken match {
     case Patterns.variableUsePattern(_) => println("We gonna be ight. -- TODO ADD TO STACK")
     case _ => print("Shit .")
   }
  }


   def innerText(): Unit = {
      Compiler.currentToken match {
        case Patterns.variableUsePattern(_) => variableUse()
        case Patterns.headingPattern(_) => heading()
        case Patterns.boldPattern(_) => bold()
        case Patterns.italicsPattern(_) => italics()
        case Patterns.listPattern(_) => listItem()
        case Patterns.imagePattern(_) => image()
        case Patterns.linkPattern(_) => link()
        case Patterns.newLinePattern(_) => lineBreak()
        case Patterns.textPattern(_) => text()
        case Patterns.paragraphBeginPattern(_) => paragraph()
        case Patterns.paragraphEndPattern(_) => paragraphEnd()
        case Patterns.variableDefPattern(_) => variableDefine()
        case _ => println("Line: " + Compiler.lineCount + " Syntax Error: Invalid Syntax: " + Compiler.currentToken)
                  setError()
      }
  }

  def innerItem() : Unit = {
    while(!Compiler.currentToken.equalsIgnoreCase("\\n")) {
      Compiler.currentToken match {
        case Patterns.variableUsePattern(_) => variableUse()
        case Patterns.boldPattern(_) => bold()
        case Patterns.italicsPattern(_) => italics()
        case Patterns.linkPattern(_) => link()
        case Patterns.textPattern(_) => text()
        case _ => Compiler.Scanner.getNextToken()
      }
      Compiler.Scanner.getNextToken()
    }
  }

  def text() : Unit = {

    if(Compiler.debugMode)
      println("Found Text -- TODO IMPLEMENT STACK")
  }
  def lineBreak() : Unit = {
    if(Compiler.debugMode)
      println("VALID LINEBREAK --- TODO ADD STACK IMPLEMENTATION")
  }

  def address() : Unit = {
    if(Compiler.debugMode)
      println("VALID ADDRESS --- TODO ADD STACK IMPLEMENTATION")
  }

   def bold(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
     if (Compiler.debugMode)
    println("Valid BOLD, ---- TODO ADD STACK IMPLEMENTATION")
  }

   def italics(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
     if (Compiler.debugMode)
    println("Valid ITALIC, ---- TODO ADD STACK IMPLEMENTATION")
  }

   def heading(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
     if (Compiler.debugMode)
    println("Valid Header, ---- TODO ADD STACK IMPLEMENTATION")
  }

   def variableDefine(): Boolean = {
    if (Compiler.currentToken.equalsIgnoreCase("\\title[")) {
      if (Compiler.debugMode)
        println("\\TITLE[ going to variableDefine() ... aka no optional variable, moving along....")
      return false
    }
     if(Compiler.debugMode)
        println("VALID DEFINE --- TODO ADD STACK")
    Compiler.currentToken match {
      case Patterns.variableDefPattern(_) => true
      case _ => false
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
    val totalMatches = Patterns.textPattern.findAllIn(text)
    if(totalMatches.size == 1) // we only should find 1 match per text token (no whitespace between)
      {
        return true
      }
    return false
  }







   def listItem(): Unit = {
     innerItem()
     Compiler.Scanner.getNextToken()
     while(!Compiler.currentToken.equalsIgnoreCase("\\n"))
     {
       listItem()
     }
   }


   def link(): Unit = {
     if (Compiler.debugMode)
  println("Valid LINK, ---- TODO ADD STACK IMPLEMENTATION")
   }

   def image(): Unit = {
     if (Compiler.debugMode)
       println("Valid IMAGE, ---- TODO ADD STACK IMPLEMENTATION")
   }

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