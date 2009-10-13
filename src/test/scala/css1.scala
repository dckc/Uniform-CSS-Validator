package org.w3.cssval

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

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


