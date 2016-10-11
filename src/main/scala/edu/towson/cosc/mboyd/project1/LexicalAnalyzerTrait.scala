package edu.towson.cosc.mboyd.project1

/**
  * Created by alex on 10/11/16.
  */
trait LexicalAnalyzerTraits {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup(candidateToken : String) : Boolean
}
