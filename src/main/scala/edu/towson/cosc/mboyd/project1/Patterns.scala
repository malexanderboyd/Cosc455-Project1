package edu.towson.cosc.mboyd.project1

/**
  * Created by Boyd on 10/31/2016.
  */
object Patterns {
  // Regex to find token patterns

  val textPattern = """([A-Za-z0-9\,\.\"\:\?_\/]+)""".r

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
  val unorderedListPattern = """(\+([a-zA-Z_0-9\s\']+))""".r // + listItem

  //Italics
  val italicsPattern = """(\*[a-zA-Z_0-9\s]+\*)""".r// * text *

  // Bold
  val boldPattern = """(\*\*[a-zA-Z_0-9\s]+\*\*)""".r// ** text **

  // Headings
  val headingPattern = """(\#[a-zA-Z_0-9\s]+)""".r

  //  The only allowed plain text in our language is: A-Z, a-z, 0-9, commas, period, quotes, colons, question marks, underscores and forward slashes.
  val generalTextPattern = """([A-Za-z0-9\,\.\"\:\?_\/])""".r

  val generalMultiTextPattern = """[A-Za-z0-9\,\.\"\:\?_\/]+""".r
}
