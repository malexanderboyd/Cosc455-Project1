package edu.towson.cosc.mboyd.project1

class SyntaxAnalyzer {

  var errorFound: Boolean = false
  var optDef: Boolean = false
  var parabcounter : Integer = 0
  var pareEcounter : Integer = 0 // lol pls dont have 9999 parab and parae. Terrible way to implement this to check if para end and start, but these should be equal if correct.
  var resolvedStack = new scala.collection.mutable.Stack[String]
  var ListOver : Boolean = false
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
    if(Compiler.debugMode)
    println("CURRENT TOKEN: " + Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {

      // add to parseTree
      // add here
      if (Compiler.debugMode)
        println("VALID BEGIN STATEMENT -- GittexBegin()")
      resolvedStack.push(Compiler.currentToken)


    } else {
      println("Line: " + Compiler.lineCount + ": SYNTAX ERROR: Expected \\BEGIN at start of input when " + Compiler.currentToken + " was found.")
      setError()
    }
  }

  def gittexEnd(): Boolean = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      resolvedStack.push(Compiler.currentToken)
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
    resolvedStack.push(Compiler.currentToken)
    hasTitle
  }
  def body(): Unit = {

    if(variableDefine()) {}
    else
      innerText()
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
    resolvedStack.push(Compiler.currentToken)

  }

  def paragraphEnd() : Unit = {
    pareEcounter += 1
    if(pareEcounter > parabcounter)
      {
        println("Line " + Compiler.lineCount + " SYNTAX ERROR: Cannot have a \\pare without prematching \\parb.")
        setError()
      }
    if(Compiler.debugMode)
      println("VALID PARE FOUND -- ADD STACK")
    resolvedStack.push(Compiler.currentToken)

  }

 def variableUse(): Unit = {
   Compiler.currentToken match {
     case Patterns.variableUsePattern(_) => if(Compiler.debugMode)
                                            println("We gonna be ight. -- TODO ADD TO STACK")
                                            resolvedStack.push(Compiler.currentToken)
     case _ => print("Major Error: " + Compiler.currentToken)
   }
  }

  def comment() : Unit = {
var hasAddress : Boolean = false
    // we need to make sure that there is an address after this to begin a link.
    val commentBuffer : String = Compiler.currentToken
    Compiler.Scanner.getNextToken()
    Compiler.currentToken = commentBuffer + Compiler.currentToken
    innerText()
  }

   def innerText(): Unit = {
     Compiler.currentToken = Compiler.currentToken.filter(!" ".contains(_))
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
        case Patterns.commentPattern(_) => comment()
        case "" => if(Compiler.debugMode)
          println("FOUND VALID SPACE IN BODY, ADDTO STACK----")//add space to stack within list
          resolvedStack.push(Compiler.currentToken)
        case "\\n" => EOL()
        case Patterns.endPattern(_) => resolvedStack.push("\\end")
        case _ => println("Line: " + Compiler.lineCount + " Lexical Error: Unknown Lexical: " + Compiler.currentToken)
                  setError()
      }
  }

  def innerItem() : Unit = {
    while(!Compiler.currentToken.equalsIgnoreCase("\\n") && !ListOver) {
      Compiler.currentToken match {
        case Patterns.variableUsePattern(_) => variableUse()
        case Patterns.boldPattern(_) => bold()
        case Patterns.italicsPattern(_) => italics()
        case Patterns.linkPattern(_) => link()
        case Patterns.textPattern(_) => text()
        case Patterns.listPattern(_) => ListOver = true
        case "" => if(Compiler.debugMode)
                      println("FOUND VALID SPACE IN LIST, ADDTO STACK----")//add space to stack within list
                        resolvedStack.push(Compiler.currentToken)
        case _ => ListOver = true

      }
      if(!ListOver)
        Compiler.Scanner.getNextToken()
    }
  }

  def text() : Unit = {

    if(Compiler.debugMode)
    println("Found Text -- TODO IMPLEMENT STACK")
    resolvedStack.push(Compiler.currentToken)
  }
  def lineBreak() : Unit = {
    if(Compiler.debugMode)
      println("VALID LINEBREAK --- TODO ADD STACK IMPLEMENTATION")
    resolvedStack.push(Compiler.currentToken)
  }

  def address() : Unit = {
    if(Compiler.debugMode)
      println("VALID ADDRESS --- TODO ADD STACK IMPLEMENTATION")
    resolvedStack.push(Compiler.currentToken)
  }

   def bold(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
     if (Compiler.debugMode)
    println("Valid BOLD, ---- TODO ADD STACK IMPLEMENTATION")
     resolvedStack.push(Compiler.currentToken)
  }

   def italics(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
     if (Compiler.debugMode)
    println("Valid ITALIC, ---- TODO ADD STACK IMPLEMENTATION")
     resolvedStack.push(Compiler.currentToken)
  }

   def heading(): Unit = {
    // We know that the pattern is correct (b/c we got here)
    // add to stack to convert to html and move on
     if (Compiler.debugMode)
    println("Valid Header, ---- TODO ADD STACK IMPLEMENTATION")
     resolvedStack.push(Compiler.currentToken)
  }

   def variableDefine(): Boolean = {
    if (Compiler.currentToken.equalsIgnoreCase("\\title[")) {
      if (Compiler.debugMode)
        println("\\TITLE[ going to variableDefine() ... aka no optional variable, moving along....")
      return false
    }
    Compiler.currentToken match {
      case Patterns.variableDefPattern(_) => if(Compiler.debugMode)
                                              println("VALID DEFINE --- TODO ADD STACK")
                                              resolvedStack.push(Compiler.currentToken)
                                              true
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
     while(!Compiler.currentToken.equalsIgnoreCase("\\n") && !ListOver)
     {
       listItem()
     }
     resolvedStack.push("+")
   }


   def link(): Unit = {
     if (Compiler.debugMode)
  println("Valid LINK, ---- TODO ADD STACK IMPLEMENTATION")
     resolvedStack.push(Compiler.currentToken)
   }

   def image(): Unit = {
     if (Compiler.debugMode)
       println("Valid IMAGE, ---- TODO ADD STACK IMPLEMENTATION")
     resolvedStack.push(Compiler.currentToken)
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