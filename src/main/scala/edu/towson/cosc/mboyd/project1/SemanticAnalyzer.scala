package edu.towson.cosc.mboyd.project1

import java.io.FileWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by alex on 10/11/16.
  */
class SemanticAnalyzer {

  var inputStack = new mutable.Stack[String]
  var ResolvedStack = new mutable.Stack[String]
  val definedVariables = new mutable.HashMap[String,String]()
  var currentLexeme: String = ""
  var fileName : String = ""
  var scopeStorage = new mutable.HashMap[String,String]()
  var scopeNames = new ArrayBuffer[String](100)
  var listItem : String = "z"
  def convert(parseStack: mutable.Stack[String]): String = {
    fileName = Compiler.fileName
    inputStack = parseStack
    while (!inputStack.isEmpty) {
      currentLexeme = inputStack.pop()
      currentLexeme match {

        //headers
        case Patterns.beginPattern(_) => generateBegin()
        case Patterns.variableDefPattern(_) => findValues()
        case Patterns.titlePattern(_) => generateTitle()
        //

        //body

        case Patterns.variableUsePattern(_) => generateUse()
        case Patterns.headingPattern(_) => generateHeading()
        case Patterns.boldPattern(_) => generateBold()
        case Patterns.italicsPattern(_) => generateItalics()
        case Patterns.listPattern(_) => generateList()
                                        while(!listItem.equalsIgnoreCase("") && !inputStack.isEmpty && !listItem.equalsIgnoreCase("+"))
                                        {
                                         listItem = inputStack.pop
                                          listItem match {
                                            case "+" => generateList()
                                            case "" => generateSpace()
                                            case _ => ResolvedStack.push(listItem)
                                          }

                                        }// need to implement this better
                                        inputStack.push("<li>")
        case Patterns.imagePattern(_) => generateImage()
        case Patterns.linkPattern(_) => generateLink()
        case Patterns.newLinePattern(_) => generateLineBreak()
        case Patterns.textPattern(_) => addText()
        case Patterns.paragraphBeginPattern(_) => generateParagraphBegin()
        case Patterns.paragraphEndPattern(_) => generateParagraphEnd()
        case "<li>" => ResolvedStack.push("</li>")
        case "" =>
        // end
        case Patterns.endPattern(_) => generateEnd()
        case _ => println("Unfound match somehow got here? oh noes: " + currentLexeme)
      }
    }

    output()
    fileName
  }
def resetVars(): Unit = {
  var i : Int = 0
  definedVariables.clear()
    while(i < scopeNames.length)
      {
        definedVariables += (scopeNames(i) -> scopeStorage(scopeNames(i)))
        i += 1
      }
}

 def storeVariables(vName : String, valValue : String) : Unit = {
   scopeStorage += (vName -> valValue)
   scopeNames += vName
  }
def generateSpace(): Unit = {
  ResolvedStack.push(" ")
}
  def output() : Unit = {
    val fileWriter = new FileWriter(fileName + ".html", true)
    while (!ResolvedStack.isEmpty) {
      currentLexeme = ResolvedStack.pop()
      currentLexeme match {
        case Patterns.variableDefPattern(_) => findRunTimeValues()
        case Patterns.variableUsePattern(_) => fileWriter.append(retrieveValue())
        case "" => fileWriter.append(" ")
        case "</p>" => fileWriter.append(currentLexeme)
                       resetVars()
        case "\\n" => generateLineBreak()
        case _ => fileWriter.append(currentLexeme)
      }
    }
    fileWriter.close()
  }

  def findRunTimeValues() : Unit = {
    var defToken : String = currentLexeme
    defToken = defToken.filter(!" ".contains(_))
    val valName : String = defToken.substring(defToken.indexOf("[") + 1, defToken.indexOf("="))
    val valValue : String =  defToken.substring(defToken.indexOf("=") + 1, defToken.indexOf("]"))
    if(definedVariables.contains(valName))
    {
      definedVariables.put(valName, valValue)
    }
    else {
      definedVariables += (valName -> valValue)
      storeVariables(valName, valValue)
      scopeNames += valName
    }
  }

  def generateEnd() : Unit ={
      ResolvedStack.push("</html>")
  }

  def generateParagraphEnd() : Unit = {
      ResolvedStack.push("</p>")
  }

  def generateParagraphBegin(): Unit = {
    ResolvedStack.push("<p>")
  }

  def addText(): Unit =
  {
    ResolvedStack.push(currentLexeme)
    ResolvedStack.push(" ")
  }


  def generateLineBreak() = {
    ResolvedStack.push("<br/>")
  }


def generateLink() : Unit = {
  var linkLex : String = currentLexeme
   // [The Simpsons])(http:..) = <a href="link">Text</a>
  ResolvedStack.push("<a href=\"" + linkLex.substring(linkLex.indexOf("(") + 1, linkLex.indexOf(")")) + "\">" + linkLex.substring(linkLex.indexOf("[") + 1, linkLex.indexOf("]")) + "</a>")
}


  def generateImage(): Unit ={
      var imgLex : String = currentLexeme
    imgLex = imgLex.filter(!"!".contains(_))
    ResolvedStack.push("<img src=\"" + imgLex.substring(imgLex.indexOf("(") + 1, imgLex.indexOf(")")) + "\" alt=\"" + imgLex.substring(imgLex.indexOf("[") + 1, imgLex.indexOf("]")) + "\">")
    //![Name](link) = <img src="link" alt="name">
      }

  def generateList() = {
    ResolvedStack.push("<li>")
  }



  def generateBold() : Unit = {
    ResolvedStack.push("</b>")
    ResolvedStack.push(currentLexeme.filter(!"**".contains(_)))
    ResolvedStack.push("<b>")
  }

def generateItalics() : Unit = {
  ResolvedStack.push("</i>")
  ResolvedStack.push(currentLexeme.filter(!"*".contains(_)))
  ResolvedStack.push("<i>")
}




  def generateDef() : Unit = {
    ResolvedStack.push(currentLexeme)
  }
  def generateUse() : Unit = {
    ResolvedStack.push(currentLexeme)
  }
  def retrieveValue(): String =
  {
    val fileWriter = new FileWriter(fileName + ".html", true)
    var useToken : String = currentLexeme
    val varName = useToken.substring(useToken.indexOf("[") + 1, useToken.indexOf("]"))
    if(definedVariables.contains(varName))
      {
        definedVariables(varName)
      }
    else
      {
        println("Semantic Error: Attempting to use undefined variable: \'" + varName + "\'.")
        System.exit(-1)
        ""
      }
  }

  def findValues() : Unit = {

    var defToken : String = currentLexeme
    defToken = defToken.filter(!" ".contains(_))
    val valName : String = defToken.substring(defToken.indexOf("[") + 1, defToken.indexOf("="))
    val valValue : String =  defToken.substring(defToken.indexOf("=") + 1, defToken.indexOf("]"))
      definedVariables += (valName -> valValue)
      storeVariables(valName, valValue)
      scopeNames += valName
    generateDef()
  }

  def generateBegin(): Unit = {
    ResolvedStack.push("<html>")
  }

  def generateTitle(): Unit = {
    ResolvedStack.push("</head>")
    ResolvedStack.push("</title>")
    ResolvedStack.push(currentLexeme.substring(currentLexeme.indexOf("[") + 1, currentLexeme.indexOf("]")))
    ResolvedStack.push("<title>")
    ResolvedStack.push("<head>")
  }

  def generateHeading(): Unit = {
    ResolvedStack.push("</h1>")
    ResolvedStack.push(currentLexeme.filter(!"#".contains(_)))
    ResolvedStack.push("<h1>")

  }
}
