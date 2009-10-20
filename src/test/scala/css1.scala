package org.w3.cssval

// http://www.scalatest.org/

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class MacroSpec extends Spec with ShouldMatchers {
  val t = new  RegexMacros {
    override val map = Map("name" -> "Bob", "age" -> "23")
  }

  describe("Macro handling") {
    it("should expand macros") {
      (t.expand("Hi. My name is {name}. I am {age} years old.")
       should equal ("Hi. My name is (?:Bob). I am (?:23) years old."))
      
    }

    it("should handle undefined macro names") {
      (t.expand("Hi. My name is {name}. I am {ageX} years old.")
       should equal ("Hi. My name is (?:Bob). I am {ageX} years old."))
      
    }
  }
}

class CSSTokens extends CSSLex {
  def tokensXXX: Parser[Any] = rep (
    pIDENT |
    `:` |
    `{` |
      `}` |
    S
    )

  def tokens: Parser[Any] = rep (
    pIDENT |
    pATKEYWORD |
    pSTRING |
    pINVALID |
    pHASH |
    pPERCENTAGE |
    pDIMENSION |
    // after PERCENTAGE and DIMENSION to implement longest-matching rule
    pNUMBER |
    pURI |
    pUNICODE_RANGE |
    CDO |
    CDC |
    `:` |
    `;` |
    `{` |
      `}` |
    `(` |
      `)` |
    `[` |
      `]` |
    S |
    COMMENT |
    pFUNCTION |
    INCLUDES |
    DASHMATCH |
    DELIM
  )
}

class CSSSyntaxSpec extends Spec with ShouldMatchers {
  val testdata = """
  blockquote { text-align: right }
  """

  describe("CSS Lexer") {

    // trying to ape http://www.scalatest.org/getting_started_with_spec
    // but support for pending tests was only added in 1.0,
    // which doesn't work with sbt yet.

    ignore("should handle \000 in strings") { }
    ignore("should handle escaped newlines in strings") { }
    ignore("should use the longest-matching rule") { }

    it("should handle noop escapes in identifiers, per example in 4.1.3") {
      val l = new CSSTokens;

      (l.parseAll(l.pIDENT, "te\\st").get
       should equal (IDENT("test")) )
    }


    it("should handle numbers") {
      val l = new CSSTokens;

      (l.parseAll(l.tokens, "1 1.2").get
       should equal (List(NUMBER(1.0), NUMBER(1.2))) )
    }

    it("should split the input into tokens") {
      val l = new CSSTokens;

      (l.parseAll(l.tokens, testdata).get
       should equal (List(IDENT("blockquote"),
			  "{",
			  IDENT("text-align"),
			  ":",
			  IDENT("right"),
			  "}") ) )

      (l.parseAll(l.tokens,
		  """identifier @keyword "string1" 'string2' #hash 1 1.2 """
		  + """10% 10.5% 12pt 12.5pt""").get
       should equal (List(IDENT("identifier"),
			  ATKEYWORD("keyword"),
			  STRING("string1"),
			  STRING("string2"),
			  HASH("hash"),
			  NUMBER(1.0),
			  NUMBER(1.2),
			  PERCENTAGE(10.0),
			  PERCENTAGE(10.5),
			  DIMENSION(12.0, "pt"),
			  DIMENSION(12.5, "pt") )) )
    }

  }

  describe("CSS Core") {

    it("should handle a basic bit of CSS syntax") {
      val css = new CSSCore;

      (css.parseAll(css.stylesheet, testdata).toString()
       should equal ("[3.3] parsed: List((((((Some(List(IDENT(blockquote)))~{)~List(((List()~((((IDENT(text-align)~List())~:)~List())~List(IDENT(right))))~List())))~List())~})~List()))") )
    }
  }
}


