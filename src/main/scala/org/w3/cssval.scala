// Uniform CSS Validator 
// Copyright (c) 2009 W3C (MIT, ERCIM, Keio)
// W3C Software license.
// Share and enjoy.


// References:
// Cascading Style Sheets Level 2 Revision 1 (CSS 2.1) Specification
// W3C Candidate Recommendation 08 September 2009
//    http://www.w3.org/TR/2009/CR-CSS2-20090908 
// esp section 4 Syntax and basic data types
// http://www.w3.org/TR/CSS2/syndata.html
/*
 * Programming in Scala
 * chapter 31 on Parsing
 * section 24.7 Regular expressions
 */


package org.w3.cssval

import scala.util.parsing.combinator.{Parsers, RegexParsers}

trait RegexMacros {
  val braces = """\{(\w+)\}""".r

  val map: Map[String, String];

  def expand(s: String): String = {
    (braces findFirstMatchIn s) match {
      case None =>
	return s
      case Some(m) =>
	// TODO: handle case of name not in map
	val n = m.group(1)
	return (if (map.contains(n))
		expand(m.before + "(?:" + map(n) + ")" + m.after)
		else s)
    }
  }
}

abstract class Token
// i prefix disambiguates (i for interpretation)
case class iIDENT(name: String) extends Token
case class iSTRING(value: String) extends Token
case class iHASH(name: String) extends Token
case class iNUMBER(value: Double) extends Token // hmm... Double?
case class iPERCENTAGE(value: Double) extends Token
case class iDIMENSION(value: Double, dim: String) extends Token
case class iURI(i: String) extends Token
case class iUNICODE_RANGE(min: Int, max: Int) extends Token
case class iFUNCTION(name: String) extends Token

class CSSLex extends RegexParsers with RegexMacros {
  /* TODO: build this map from macro_table below */
  override val map: Map[String, String] = Map(
    "ident" -> "[-]?{nmstart}{nmchar}*",
    "name" -> 	"{nmchar}+",
    "nmstart" -> "[_a-z]|{nonascii}|{escape}",
    "nonascii" ->  """[^\00-\0177]""", // \0 -> \00 \177 -> \0177 for Java
    "unicode" ->   """\\[0-9a-f]{1,6}({unicode_x}|[ \n\r\t\f])?""",
    // precedence of | is different in lex and regex, hence these _1 macros
    "unicode_x" -> """\r\n""",
    "escape" ->	"""{unicode}|{escape_x}""",
    "escape_x" -> """\\[^\n\r\f0-9a-f]""",
    "nmchar" -> "[_a-z0-9-]|{nonascii}|{escape}",
    "num" -> """[0-9]+|{num_x}""",
    "num_x" -> """[0-9]*\.[0-9]+""",
    "string" -> "{string1}|{string2}",
    "string1" -> ("""\"([^\n\r\f\\"]|{nl_x}|{escape})*""" + "\\\""),
    "string2" -> """\'([^\n\r\f\\']|{nl_x}|{escape})*\'""",
    "invalid" -> "{invalid1}|{invalid2}",
    "invalid1" -> """\"([^\n\r\f\\"]|{nl_x}|{escape})*""",
    "invalid2" -> """\'([^\n\r\f\\']|{nl_x}|{escape})*""",
    "nl" -> """\n|{nl_y}|\r|\f""",
    "nl_x" -> """\\{nl}""",
    "nl_y" -> """\r\n""",
    "w" -> """[ \t\r\n\f]*""")

  val macro_table = """
ident 	[-]?{nmstart}{nmchar}*
name 	{nmchar}+
nmstart 	[_a-z]|{nonascii}|{escape}
nonascii	[^\0-\177]
unicode 	\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
escape 	{unicode}|\\[^\n\r\f0-9a-f]
nmchar 	[_a-z0-9-]|{nonascii}|{escape}
num 	[0-9]+|[0-9]*\.[0-9]+
string 	{string1}|{string2}
string1 	\"([^\n\r\f\\"]|\\{nl}|{escape})*\"
string2 	\'([^\n\r\f\\']|\\{nl}|{escape})*\'
invalid 	{invalid1}|{invalid2}
invalid1	\"([^\n\r\f\\"]|\\{nl}|{escape})*
invalid2	\'([^\n\r\f\\']|\\{nl}|{escape})*
nl 	\n|\r\n|\r|\f
w 	[ \t\r\n\f]*
""";

  val IDENT = tok("{ident}") ^^ {
    case s =>
      val n = unescape(s)
      iIDENT(n)
  }
  val ATKEYWORD = tok("@{ident}")
  val STRING = tok("{string}")
  val INVALID = tok("{invalid}")
  val HASH = tok("#{name}")
  val NUMBER = tok("{num}")
  val PERCENTAGE = tok("{num}%")
  val DIMENSION = tok("{num}{ident}")
			// extra ()s to bind | to the right level
  val URI = tok("""(url\({w}{string}{w}\))""" +
		"""|(url\({w}([!#$%&*-~]|{nonascii}|{escape})*{w}\))""")
  val UNICODE_RANGE = tok("""u\+[0-9a-f?]{1,6}(-[0-9a-f]{1,6})?""")
  val CDO = tok("<!--")
  val CDC = tok("-->")
  val `:` = tok(":")
  val `;` = tok(";")
  val `{` = tok("""\{""")
  val `}` = tok("""\}""")
  val `(` = tok("""\(""")
  val `)` = tok("""\)""")
  val `[` = tok("""\[""")
  val `]` = tok("""\]""")
  val S_ = """[ \t\r\n\f]+"""
  val S = tok(S_)
  // @@TODO: override def whiteSpace = S_.r
  // override def skipWhitespace = true
  val COMMENT = tok("""\/\*[^*]*\*+([^/*][^*]*\*+)*\/""")
  val FUNCTION = tok("""{ident}\(""")
  val INCLUDES = tok("~=")
  val DASHMATCH = tok("|=")
  val DELIM = tok("@@ any other character not matched by the above rules, and neither a single nor a double quote")

  def tok(s: String): Parser[String] = {
    expand(s).r
  }

  /*
   * In CSS 2.1, a backslash (\) character indicates three types of
   * character escapes. 
   */
  def unescape(s: String): String = {
    // this could probably be optimized, using StringBuilder or some such...
    val re = ("""\\(?:(\n)""" +
	      """|(?:([0-9a-f]{1,6})(?:(?:\r\n)|[ \n\r\t\f])?)""" +
	      """|([^\n\r\f0-9a-f]))""").r
    (re findFirstMatchIn s) match {
      case None =>
	return s
      case Some(m) =>
	val (nl, hex, chr) = (m.group(1), m.group(2), m.group(3))
	  /* First, inside a string, a backslash followed by a newline
	   * is ignored */
        val e = (if (nl != null)  ""
          /* Second, it cancels the meaning of special CSS characters.*/
		 else if (chr != null) chr
          /* Third, backslash escapes allow authors to refer to
           * characters they cannot easily put in a document. In this
           * case, the backslash is followed by at most six
           * hexadecimal digits (0..9A..F), which stand for the ISO
           * 10646 ([ISO10646]) character with that number, which must
           * not be zero. */
		 else
		   //TODO: check for 0
		   new String(Character.toChars(Integer.parseInt(hex, 16)))
		 )
        m.before + e + unescape(m.after.toString())
    }
  }
}


class CSSCore extends CSSLex {

/*
 * core syntax for CSS
 * http://www.w3.org/TR/CSS2/syndata.html
 *
stylesheet  : [ CDO | CDC | S | statement ]*;
statement   : ruleset | at-rule;
at-rule     : ATKEYWORD S* any* [ block | ';' S* ];
block       : '{' S* [ any | block | ATKEYWORD S* | ';' S* ]* '}' S*;
ruleset     : selector? '{' S* declaration? [ ';' S* declaration? ]* '}' S*;
selector    : any+;
declaration : property S* ':' S* value;
property    : IDENT;
value       : [ any | block | ATKEYWORD S* ]+;
any         : [ IDENT | NUMBER | PERCENTAGE | DIMENSION | STRING
              | DELIM | URI | HASH | UNICODE-RANGE | INCLUDES
              | DASHMATCH | ':' | FUNCTION S* any* ')' 
              | '(' S* any* ')' | '[' S* any* ']' ] S*;
 */

  def stylesheet: Parser[Any] = rep ( CDO | CDC | S | statement )

  def statement: Parser[Any] = ruleset // | at-rule

  def ruleset: Parser[Any] = (opt(selector) ~ "{" ~ 
		repsep(rep(S) ~ declaration ~ rep(S), ";") ~
		rep(S) ~ "}" ~ rep(S) )
  def selector: Parser[Any] = rep1(any)
  def declaration: Parser[Any] = property ~ rep(S) ~ ":" ~ rep(S) ~ value
  def property: Parser[Any] =  IDENT
  def value: Parser[Any] = rep1( any 
			       // TODO: | block | ATKEYWORD rep(S)
			     )
  def any: Parser[Any] = ( IDENT  // TODO IDENT | NUMBER ...
	   ) ~ rep(S)
}

class CSSParser extends CSSCore {
  def main (args: Array[String]) {
    println("input: " + args(0))
    println(parseAll(stylesheet, args(0)))
  }
}
