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

package org.w3.cssval

import scala.util.parsing.combinator.{Parsers, RegexParsers}

trait RegexMacros {
  // see book section 24.7 Regular expressions
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

  val IDENT = tok("{ident}")
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
  val S = tok("""[ \t\r\n\f]+""")
  val COMMENT = tok("""\/\*[^*]*\*+([^/*][^*]*\*+)*\/""")
  val FUNCTION = tok("""{ident}\(""")
  val INCLUDES = tok("~=")
  val DASHMATCH = tok("|=")
  val DELIM = tok("@@ any other character not matched by the above rules, and neither a single nor a double quote")

  def tok(s: String): Parser[String] = {
    expand(s).r
  }

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

}


class CSSCore extends CSSLex {

/* cf. Programming in Scala chapter 31 */

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
