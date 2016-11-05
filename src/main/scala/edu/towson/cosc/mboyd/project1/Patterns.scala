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


  //Italics
  val italicsPattern = """(\*[a-zA-Z_0-9\s]+\*)""".r// * text *

  // Bold
  val boldPattern = """(\*\*[a-zA-Z_0-9\s]+\*\*)""".r// ** text **

  // Headings
  val headingPattern = """(\#[a-zA-Z_0-9\s]+)""".r

  val commentPattern = """(\[.+\])""".r
  val addressPattern = """(\(.+\))""".r

  val imagePattern = """(\!\s?\[.+\]\s?\(.+\)?\s?)""".r // not sure if using this to check if it's correct or slicing it up and compare versus previous patterns if better

  val newLinePattern = """(\\\\)""".r


  val linkPattern = """(\[.+\]\(.+\))""".r

  val listPattern = """(\+.?)""".r

  val paragraphBeginPattern = """(\\PARB)""".r
  val paragraphEndPattern = """(\\PARE)""".r
  //    //case '+' => getList()
  //case '\\' => identifySlash()
  //case '(' => getAddress()
  //case '!' => getImage()









  //  The only allowed plain text in our language is: A-Z, a-z, 0-9, commas, period, quotes, colons, question marks, underscores and forward slashes.
  val generalTextPattern = """([A-Za-z0-9\,\.\"\:\?_\/])""".r

  val generalMultiTextPattern = """[A-Za-z0-9\,\.\"\:\?_\/]+""".r



}
