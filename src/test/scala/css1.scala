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
  def tokens: Parser[Any] = rep (
    pIDENT |
    `:` |
    `{` |
      `}` |
    S
    )

  def tokensXXX: Parser[Any] = (
    pIDENT |
    pATKEYWORD |
    pSTRING |
    pINVALID |
    pHASH |
    pNUMBER |
    pPERCENTAGE |
    pDIMENSION |
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
    it("should split the input into tokens") {
      val l = new CSSTokens;

      (l.parseAll(l.tokens, testdata).toString()
       should equal (
	 "[3.3] parsed: List(IDENT(blockquote), {, IDENT(text-align), :, IDENT(right), })") )
    }

    it("should handle noop escapes in identifiers, per example in 4.1.3") {
      val l = new CSSTokens;

      (l.parseAll(l.pIDENT, "te\\st").toString()
       should equal ("""[1.6] parsed: IDENT(test)""") )
    }


    // @@ it("should handle escaped newlines in strings") is pending()
  }

  describe("CSS Core") {

    it("should handle a basic bit of CSS syntax") {
      val css = new CSSCore;

      (css.parseAll(css.stylesheet, testdata).toString()
       should equal ("[3.3] parsed: List((((((Some(List(IDENT(blockquote)))~{)~List(((List()~((((IDENT(text-align)~List())~:)~List())~List(IDENT(right))))~List())))~List())~})~List()))") )
    }
  }
}


