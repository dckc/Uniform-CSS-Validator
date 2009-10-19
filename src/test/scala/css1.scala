package org.w3.cssval

// http://www.scalatest.org/

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class MacroSpec extends Spec with ShouldMatchers {
  describe("Macro handling") {
    it("should expand macros") {
      val t = new  Macros {
	override val map = Map("name" -> "Bob", "age" -> "23")
      }

      (t.expand("Hi. My name is {name}. I am {age} years old.")
       should equal ("Hi. My name is Bob. I am 23 years old."))
      
    }
  }
}

class CSSSyntaxSpec extends Spec with ShouldMatchers {
  describe("CSS Syntax Oracle") {

    it("should handle a basic bit of CSS syntax") {
      val testdata = """
      blockquote { text-align: right }
      """
      val css = new CSSCore;

      (css.parseAll(css.stylesheet, testdata)
       should equal (true)) //@@
    }
  }
}


