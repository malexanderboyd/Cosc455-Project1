package edu.towson.cosc.mboyd.project1


import scala.io.Source

import java.awt.Desktop
import java.io.{File, IOException}

object Compiler {

  var currentToken: String = ""
  var lineCount = 0
  val Scanner = new LexicalAnalyzer
  val Parser = new SyntaxAnalyzer
  val Semantics = new SemanticAnalyzer
  var debugMode: Boolean = false
  var numPasses : Integer = 0
  //basic <gittex>
  var hasDocBegin: Boolean = false
  var hasOptVar: Boolean = false
  var hasTitle: Boolean = false
  var hasBody: Boolean = false
  var hasDocEnd: Boolean = false
  var fileName : String = ""
  def main(args: Array[String]) = {
    // make sure input follows usage rules
    checkInput(args)
    Compile(args(0))
    // for each line read from file, scan and parse


  }

  def getCurrentToken() : Unit = {
    if (debugMode)
      println("Current Token going to Parser: " + currentToken)
  }

  def Compile(fileName: String): Unit = {
    for (line <- Source.fromFile(fileName).getLines()) {
      // get the first Token
      if(line.length >0 && hasDocEnd)
        {
          lineCount += 1
          println("Line: " + lineCount + " Syntax Error: File has \\END declared but more input afterwards." + line)
          System.exit(-1)
        }
      var currLine : String = line
      val filteredLine : String = currLine.filter(!"\t".contains(_))
      if (!hasDocEnd) {
        if (debugMode)
          println("Current Line being Analyzed: " + filteredLine)
        Scanner.start(filteredLine)
        // keep lineCount
        lineCount += 1

        if (!hasDocBegin && numPasses == 0) {
          if (debugMode)
            println("===== Checking for Doc Begin ====")
          getCurrentToken()
          if (!Scanner.getError)
            hasDocBegin = Parser.gittexStart()
          if (debugMode)
            println("===== Status of Doc Begin:  " + hasDocBegin + " ====")
          checkForSyntaxErrors()
        }
        if (!hasOptVar && hasDocBegin && numPasses == 1) {
          if (debugMode)
            println("===== Checking for Optional Variable Def ====")
          getCurrentToken()
          if (!Scanner.getError)
            hasOptVar = Parser.variableDefine()
          if (debugMode)
            println("===== Has Optional Variable:  " + hasOptVar + " ====")
          checkForSyntaxErrors()
        }
        if ((!hasTitle && hasDocBegin && !hasOptVar && numPasses == 1) || (!hasTitle && numPasses == 2 && hasDocBegin)) {
          if (debugMode)
            println("===== Checking for Title ====")
          getCurrentToken()
          if (!Scanner.getError)
            hasTitle = Parser.Title()
          if (debugMode)
            println("===== Has Title:  " + hasTitle + " ====")
          checkForSyntaxErrors()
        }
        if (!hasBody && hasTitle && numPasses > 2 || !hasBody && numPasses == 2) {
          if (debugMode)
            println("===== Checking for Body Content ====")
          if (currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
            hasBody = true
            hasDocEnd = true
            Parser.gittexEnd()
          }
          else {
            hasBody = false
            getCurrentToken()
            if (!Scanner.getError)
              Parser.body()
          }
          checkForSyntaxErrors()
        }
        numPasses += 1
      }
    }// end of line input loop

      if (Parser.parabcounter != Parser.pareEcounter) {
        if (Parser.parabcounter > Parser.pareEcounter)
          println("SYNTAX ERROR: Paragraph does not have closing paragraph (\\PARE)")
        else
          println("SYNTAX ERROR: Paragraph does not have starting paragraph (\\PARE) found")
      }
      if (debugMode)
        println("===== Has body content: " + hasBody + " ====")
      if (!hasDocEnd && hasBody) {
        if (debugMode)
          println("===== Checking for Gittex End ====")
        if (!Scanner.getError)
          hasDocEnd = Parser.gittexEnd()
      }
    if (debugMode)
      println("===== Has Gittex End:  " + hasDocEnd + " ====")

    checkForSyntaxErrors()
    openHTMLFileInBrowser(Semantics.convert(Parser.resolvedStack))
  }


  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr + ".html")
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }




  def checkForSyntaxErrors(): Unit = {
    if (Parser.getError)
      System.exit(-1)
  }

  def checkInput(inputFile: Array[String]): Unit = {

    fileName = inputFile(0).filter(!".mkd".contains(_))
    if (inputFile.length != 1) {
      println("Usage Error: Wrong number of input arguments.")
      System.exit(0)
    }
    else if (!inputFile(0).endsWith(".mkd")) {
      println("Usage Error: Input file should be \".mkd\" extension.")
      System.exit(0)
    }

  }
}