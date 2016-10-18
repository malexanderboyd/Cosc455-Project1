package edu.towson.cosc.mboyd.project1


import scala.io.Source
object Compiler {

  var currentToken : String = ""
  var lineCount = 0
  val Scanner = new LexicalAnalyzer
  val Parser = new SyntaxAnalyzer
  var fileContents : String = ""


  def main(args: Array[String]) = {
    // make sure input follows usage rules
    checkInput(args)
    Compile(args(0))
    // for each line read from file, scan and parse


    }

  def Compile(fileName : String) : Unit = {
  for (line <- Source.fromFile(fileName).getLines()) {
  // get the first Token
  println ("Current Line being Analyzed: " + line)
  Scanner.start (line)
  // keep lineCount
  lineCount += 1
  println ("Current Token going to Parser: " + currentToken)
  Parser.gittex()

  if (Parser.getError)
  println ("Error In Parser.")
}
  }

  def checkInput(inputFile: Array[String]) : Unit = {
      if(inputFile.length != 1) {
        println("Usage Error: Wrong number of input arguments.")
        System.exit(0)
      }
      else if(!inputFile(0).endsWith(".mkd"))
      {
        println("Usage Error: Input file should be \".mkd\" extension.")
        System.exit(0)
      }

  }
}