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
    pFUNCTION |
    INCLUDES |
    DASHMATCH |
    DELIM
  )
}

class CSSSyntaxSpec extends Spec with ShouldMatchers {
  describe("CSS Lexer") {
    val l = new CSSTokens;

    // trying to ape http://www.scalatest.org/getting_started_with_spec
    // but support for pending tests was only added in 1.0,
    // which doesn't work with sbt yet.

    ignore("should use the longest-matching rule") { }

    it("should skip comments as well as whitespace per 4.1.1 Tokenization") {
      (l.parseAll(l.tokens, "h1 /* comment */ { background: blue }").get
       should equal (List(IDENT("h1"),
			  "{",
			  IDENT("background"),
			  ":",
			  IDENT("blue"),
			  "}") ) )

    }

    ignore("should do: a comment before or within the @charset rule disables the @charset.") { }

    it("""should do: Other space-like characters, such as "em-space" (U+2003) and "ideographic space" (U+3000), are never part of white space.""") {
      (l.parseAll(l.tokens, "h1 \u2003 \u3000 { background: blue }").get
       should equal (List(IDENT("h1"),
			  IDENT("\u2003"),
			  IDENT("\u3000"),
			  "{",
			  IDENT("background"),
			  ":",
			  IDENT("blue"),
			  "}")) )
    }

    it("""should do: Keywords must not be placed between quotes ("..." or '...').""") {
      (l.parseAll(l.tokens, """
		  width: "auto";
		  border: "none";
		  background: "red";
		  """).get
       should equal(List(IDENT("width"), ":", STRING("auto"), ";",
			 IDENT("border"), ":", STRING("none"), ";",
			 IDENT("background"), ":", STRING("red"), ";"))
       )
    }

    it("""should do example from 4.1.2.1 Vendor-specific extensionsa""") {
      (l.parseAll(l.tokens, """
		  -moz-box-sizing
		  -moz-border-radius
		  -wap-accesskey
		  """).get
       should equal (List(IDENT("-moz-box-sizing"),
			  IDENT("-moz-border-radius"),
			  IDENT("-wap-accesskey") )) )
    }

    it("""should do examples from 4.1.3 Characters and case""") {
      (l.parseAll(l.tokens, """B\&W\? B\26 W\3F""").get
       should equal (List(IDENT("B&W?"), IDENT("B&W?"))) )

      (l.parseAll(l.tokens, """\7B  {  \32  2""").get
       should equal (List(IDENT("{"), "{", IDENT("2"), NUMBER(2))) )
    }
    it("""should handle out-of-range escape numbers somehow, per 4.1.3""") {
      (l.parseAll(l.tokens, """\110000""").get
       should equal (List(IDENT("\uFFFD"))) )
    }

    ignore("should handle \000 in strings") { }
    ignore("should handle escaped newlines in strings") { }

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
      (l.parseAll(l.tokens, "blockquote { text-align: right }").get
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

      (css.parseAll(css.stylesheet,
		    "blockquote { text-align: right }").toString()
       should equal ("[1.33] parsed: List((((Some(List(IDENT(blockquote)))~{)~List(((IDENT(text-align)~:)~List(IDENT(right)))))~}))") )
    }
  }
}


