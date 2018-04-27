import java.io.ByteArrayInputStream

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.TableDrivenPropertyChecks._

class XMLSafetyCheckerSpec extends WordSpec with Matchers {

  val samples =
    Table(
      ("XML", "expected result"),
      ("<html></html>", true),
      ("<html><<", true),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
        |  <!DOCTYPE foo [
        |  <!ELEMENT foo ANY>] >
        |  <foo>xxx</foo>
        |  """.stripMargin,
        true),
      (
        """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
         |            "http://www.w3.org/TR/html4/strict.dtd">
         |    <html>
         |    <head>
         |    </head>
         |    <body>
         |    </body>
         |    </html>""".stripMargin,
        false),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
          |  <!DOCTYPE foo [
          |  <!ELEMENT foo ANY >
          |  <!ENTITY xxe SYSTEM "file:///does/not/exist" >]>
          |  <foo>No entity in the body</foo>
          |  """.stripMargin,
        false),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
        |  <!DOCTYPE foo [
        |  <!ELEMENT foo ANY >
        |  <!ENTITY xxe SYSTEM "file:///does/not/exist" >]>
        |  <foo><<<<
        |  """.stripMargin,
        false),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
          |  <!DOCTYPE foo [
          |  <!ELEMENT foo ANY >
          |  <!ENTITY xxez PUBLIC "-//OASIS//DTD DocBook V4.1//EN" >]>
          |  <foo><<<<
          |  """.stripMargin,
        true),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
        |  <!DOCTYPE foo [
        |  <!ELEMENT foo ANY >
        |  <!ENTITY xxe PUBLIC "-//OASIS//DTD DocBook V4.1//EN" "file:///does/not/exist2" >]>
        |  <foo><<<<
        |  """.stripMargin,
        false),
      (
        """<!DOCTYPE test [ <!ENTITY % init SYSTEM "data://text/plain;base64,ZmlsZTovLy9ldGMvcGFzc3dk"> %init; ]><foo/>
         |""".stripMargin,
        false
      )
    )

  "Should properly perform DTD safety checks (manual analyzing)" in {
    forAll(samples) { (xml: String, expectedResult: Boolean) =>
      ExplicitAnalysisXMLSafetyChecker.checkIfDtdSafe(new ByteArrayInputStream(xml.getBytes)) shouldBe expectedResult

    }
  }

  "Should properly perform DTD safety checks (relying on XMLStreamParser)" in {
    forAll(samples) { (xml: String, expectedResult: Boolean) =>
      RelyingOnExceptionXMLSafetyChecker.checkIfSafe(new ByteArrayInputStream(xml.getBytes)) shouldBe expectedResult
    }
  }

  "Should properly perform Billion Laughs safety checks (manual analyzing)" in {
    val xml = """<?xml version="1.0" encoding="ISO-8859-1"?>
      |<!DOCTYPE lolz [
      |<!ENTITY lol "lol">
      |<!ELEMENT lolz (#PCDATA)>
      |<!ENTITY lol1 "&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;">
      |<!ENTITY lol2 "&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;">
      |<!ENTITY lol3 "&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;">
      |<!ENTITY lol4 "&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;">
      |<!ENTITY lol5 "&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;">
      |<!ENTITY lol6 "&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;">
      |<!ENTITY lol7 "&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;">
      |<!ENTITY lol8 "&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;">
      |<!ENTITY lol9 "&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;">
      |]>
      |<lolz>&lol9;</lolz>""".stripMargin

    ExplicitAnalysisXMLSafetyChecker.checkIfBillionLaughsSafe(new ByteArrayInputStream(xml.getBytes)) shouldBe false
  }

}
