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
 *
 * Java regex syntax
 * http://java.sun.com/j2se/1.4.2/docs/api/java/util/regex/Pattern.html
 */


package org.w3.cssval

import java.lang.CharSequence
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

abstract class Token // sealed?
case class IDENT(name: String) extends Token
case class ATKEYWORD(name: String) extends Token
case class STRING(value: String) extends Token
case class INVALID(value: String) extends Token
case class HASH(name: String) extends Token
case class NUMBER(value: Double) extends Token // hmm... Double?
case class PERCENTAGE(value: Double) extends Token
case class DIMENSION(value: Double, dim: String) extends Token
case class URI(i: String) extends Token
case class UNICODE_RANGE(min: Int, max: Int) extends Token
case class FUNCTION(name: String) extends Token
case class DELIM(c: String) extends Token

class CSSLex extends RegexParsers with CSSMacros {
  val pIDENT: Parser[Token] = tok("{ident}") ^^ {
    case s =>
      IDENT(unescape(s).toString)
  }

  val pATKEYWORD = new RegexParts("@({ident})") ^^ {
    case m =>
      ATKEYWORD(unescape(m.group(1)).toString)
  }

  val pSTRING = tok("{string}") ^^ {
    case s =>
      STRING(unescape(s.subSequence(1, s.length()-1)).toString)
  }

  val pINVALID = tok("{invalid}")

  val pHASH: Parser[Token] = tok("#{name}") ^^ {
    case s =>
      HASH(unescape(s.substring(1)).toString)
  }

  val pNUMBER: Parser[Token] = tok("{num}") ^^ {
    case s =>
      NUMBER(s.toDouble)
  }

  val pPERCENTAGE: Parser[Token] = new RegexParts("({num})%") ^^ {
    case m =>
      PERCENTAGE(m.group(1).toDouble)
  }

  val pDIMENSION: Parser[Token] = new RegexParts("({num})({ident})") ^^ {
    case m =>
      DIMENSION(m.group(1).toDouble, m.group(2))
  }



  /* <Yves> Dan, I know you are using the general parsing rules instead of the versionned grammar, however if you use the 'url' production there, it has a bug (see http://lists.w3.org/Archives/Public/www-style/2009Oct/0206.html) */
			// extra ()s to bind | to the right level
  val pURI = tok("""(url\({w}{string}{w}\))""" +
		"""|(url\({w}([!#$%&*-~]|{nonascii}|{escape})*{w}\))""")
  val pUNICODE_RANGE = tok("""u\+[0-9a-f?]{1,6}(-[0-9a-f]{1,6})?""")
  val CDO = literal("<!--")
  val CDC = literal("-->")
  val `:` = literal(":")
  val `;` = literal(";")
  val `{` = literal("{")
  val `}` = literal("}")
  val `(` = literal("(")
  val `)` = literal(")")
  val `[` = literal("[")
  val `]` = literal("]")

  // Spec says:
  //   COMMENT tokens do not occur in the grammar (to keep it readable), ...
  // It's not clear why S isn't handled likewise. We're
  // using whiteSpace support for both of them:
  override val whiteSpace = expand("(?:{w_1}|{comment})+").r

  val pFUNCTION = tok("""{ident}\(""")
  val INCLUDES = literal("~=")
  val DASHMATCH = literal("|=")

  /* spec says: "any other character not matched by the above rules,
  and neither a single nor a double quote". Not quite sure how to do that. */
  val pDELIM: Parser[Token] = tok("[^'\"@{}():; \t\r\n\f]") ^^ {
     case c =>
      DELIM(c)
  }

  class RegexParts(defn: String) extends Parser[Match] {
    val re = expand(defn).r
    val p = regex(re)
    
    def apply(in: Input) = p(in) match {
      case Success(s, in1) =>
	// this runs the regex again. oh well.
	Success((re findPrefixMatchOf s).get, in1)
      case Failure(x, in1) =>
	new Failure(("expected match of `" + re + "' but found " + in.first),
		    in1)
      // these cases aren't exhaustive; I can't figure out what
      // to do about that. I suppose throwing a MatchError is reasonable
    }
  }


  def tok(s: String): Parser[String] = {
    expand(s).r
  }

  /*
   * In CSS 2.1, a backslash (\) character indicates three types of
   * character escapes. 
   */
  def unescape(cs: CharSequence): CharSequence = {
    // This is a purely functional implementation;
    // we'll leave optimization to the compiler until
    // performance measurements suggest otherwise.
    val re = ("""\\(?:(\n)""" +
	      """|(?:([0-9a-fA-F]{1,6})(?:(?:\r\n)|[ \n\r\t\f])?)""" +
	      """|([^\n\r\f0-9a-fA-F]))""").r
    (re findFirstMatchIn cs) match {
      case None =>
	return cs
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
		 else {
		   val i = Integer.parseInt(hex, 16)
		   val replacement = "\uFFFD"
		   if (i > 0 && i <= 0x10FFFF) new String(Character.toChars(i))
		   else replacement
		 }
	       )
        m.before + e + unescape(m.after)
    }
  }
}

trait RegexMacros {
  val braces = """\{(\w+)\}""".r

  val map: Map[String, String];

  def expand(s: String): String = {
    (braces findFirstMatchIn s) match {
      case None =>
	return s
      case Some(m) =>
	val n = m.group(1)
	return (if (map.contains(n))
		expand(m.before + "(?:" + map(n) + ")" + m.after)
		else s)
    }
  }
}

trait CSSMacros extends RegexMacros {
  /* TODO: build this map from macro_table below */
  override val map: Map[String, String] = Map(
    "ident" -> "[-]?{nmstart}{nmchar}*",
    "name" -> 	"{nmchar}+",
    // uppercase too
    "nmstart" -> "[_a-zA-Z]|{nonascii}|{escape}",
    "nonascii" ->  """[^\00-\0177]""", // \0 -> \00 \177 -> \0177 for Java
    "unicode" ->   """\\[0-9a-fA-F]{1,6}({unicode_x}|[ \n\r\t\f])?""",
    // precedence of | is different in lex and regex, hence these _1 macros
    // cf http://flex.sourceforge.net/manual/Patterns.html
    "unicode_x" -> """\r\n""",
    "escape" ->	"""{unicode}|{escape_x}""",
    "escape_x" -> """\\[^\n\r\f0-9a-fA-F]""",
    "nmchar" -> "[_a-zA-Z0-9-]|{nonascii}|{escape}",
    // reversed order of | parts due to lex/regex impedence mismatch
    // ugh... I wonder how many others are lurking
    "num" -> """{num_x}|[0-9]+""",
    "num_x" -> """[0-9]*\.[0-9]+""",
    "string" -> "{string1}|{string2}",
    "string1" -> "\"{string1_1}\"",
    "string1_1" -> """([^\n\r\f\\"]|{nl_x}|{escape})*""",
    "string2" -> "'{string2_1}'",
    "string2_1" -> """([^\n\r\f\\']|\\{nl}|{escape})*""",
    "invalid" -> "{invalid1}|{invalid2}",
    "invalid1" -> """\"([^\n\r\f\\"]|{nl_x}|{escape})*""",
    "invalid2" -> """\'([^\n\r\f\\']|{nl_x}|{escape})*""",
    "nl" -> """\n|{nl_y}|\r|\f""",
    "nl_x" -> """\\{nl}""",
    "nl_y" -> """\r\n""",
    "w" -> """[ \t\r\n\f]*""",
    // handling comments like whitespace
    "w_1" -> """[ \t\r\n\f]""",
    "comment" -> """/\*[^*]*\*+([^/*][^*]*\*+)*/"""
    )

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

  def stylesheet: Parser[Any] = rep ( CDO | CDC | statement )

  def statement: Parser[Any] = ruleset | at_rule

  def at_rule: Parser[Any] = pATKEYWORD ~ rep(any) ~ opt( block | ";" )

  def block: Parser[Any] = "{" ~ rep( any | block | pATKEYWORD | ";" ) <~ "}"

  def ruleset: Parser[Any] = (
    opt(selector) ~ ("{" ~> repsep(declaration, ";") <~ "}") )
  def selector: Parser[Any] = rep1(any)
  def declaration: Parser[Any] = property ~ (":" ~> value)
  def property: Parser[Any] =  pIDENT
  def value: Parser[Any] = rep1( any | block | pATKEYWORD )
  def any: Parser[Any] = (pIDENT
			  | pPERCENTAGE | pDIMENSION | pNUMBER
			  | pSTRING | pURI | pHASH | pUNICODE_RANGE
			  | INCLUDES | DASHMATCH | ":"
			  | (pFUNCTION ~ rep(any) <~ ")" )
			  | ("(" ~ rep(any) <~")")
			  | ("[" ~ rep(any) <~"]")
                          | pDELIM  )
}

class CSSParser extends CSSCore {
  def main (args: Array[String]) {
    println("input: " + args(0))
    println(parseAll(stylesheet, args(0)))
  }
}
