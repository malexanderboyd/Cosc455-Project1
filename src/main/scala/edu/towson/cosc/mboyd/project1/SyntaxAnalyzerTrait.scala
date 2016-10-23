package edu.towson.cosc.mboyd.project1

/**
  * Created by alex on 10/11/16.
  */
trait SyntaxAnalyzerTraits {
  def gittexStart() : Boolean
  def gittexEnd() : Boolean
  def Title() : Boolean
  def body() : Boolean
  def paragraph() : Unit
  def innerText() : Unit
  def heading() : Unit
  def variableDefine() : Boolean
  def variableDefineBegin : Unit
  def variableName : Unit
  def variableDefineEnd : Unit
  def variableUse() : Unit
  def bold() : Unit
  def italics() : Unit
  def listItem() : Unit
  def innerItem() : Unit
  def link() : Unit
  def image() : Unit
  def newline() : Unit
  def EOL() : Unit
}
