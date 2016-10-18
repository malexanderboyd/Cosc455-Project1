package edu.towson.cosc.mboyd.project1
import scala.collection.mutable.ListBuffer
/**
  * Created by alex on 10/11/16.
  */
object CONSTANTS {
  /*
      FOLLOW OFFICIAL BNF TO REFINE THIS

   */

  val DOCB : String = "\\BEGIN"
  val DOCE : String = "\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE : String = "["
  val HEADING : String = "#"
  val PARAB : String = "\\PARAB"
  val PARAE : String = "\\PARAE"
  val BOLD : String = "**"
  val ITALICS : String = "*"
  val LISTITEM : String = "+"
  val NEWLINE : String = "\\\\"
  val LINKB : String = "["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = "\\DEF["
  val EQSIGN : String = "="
  val USEB : String = "\\USE["
  val EOL : Char = '\n'
  val EOLS : String = "\n"
  val validLexemes = new ListBuffer[String]()

  validLexemes += DOCB
  validLexemes += DOCE
  validLexemes += TITLEB
  validLexemes += BRACKETE
  validLexemes += HEADING
  validLexemes += PARAB
  validLexemes += PARAE
  validLexemes += BOLD
  validLexemes += ITALICS
  validLexemes += LISTITEM
  validLexemes += NEWLINE
  validLexemes += LINKB
  validLexemes += ADDRESSB
  validLexemes += ADDRESSE
  validLexemes += IMAGEB
  validLexemes += DEFB
  validLexemes += EQSIGN
  validLexemes += USEB
  validLexemes += "\n"


  /* REGEX of each possible Token
  import scala.util.matching.Regex
  val beginRx : String = "\\BEGIN"
  val titleRx = new Regex("\\\\TITLE\\[.+\\]")
  val test = titleRx findAllIn(candidateToken)
  println("regex test: " + test.size) // test.size will return 1 if correctly matches (should only find 1 \begin
  */



}
