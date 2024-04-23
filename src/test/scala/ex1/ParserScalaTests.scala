package ex1

import org.scalatest.matchers.should.Matchers.*
import ex1.Parsers.*
import org.scalatest.flatspec.AnyFlatSpec

class ParserScalaTests extends AnyFlatSpec:
    "Basic parser" should "accept only right strings" in:
        def parser = new BasicParser(Set('a', 'b', 'c'))
        parser.parseAll("aabc".toList) shouldBe true
        parser.parseAll("aabcdc".toList) shouldBe false
        parser.parseAll("".toList) shouldBe true

    "Not Empty parser" should "accept only not empty right strings" in:
        def parserNE = new NonEmptyParser(Set('0', '1'))
        parserNE.parseAll("0101".toList) shouldBe true
        parserNE.parseAll("0123".toList) shouldBe false
        parserNE.parseAll("".toList) shouldBe false

    "Not Two consecutive parser" should "accept only right strings without two consecutive chars" in:
        def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
        parserNTC.parseAll("XYZ".toList) shouldBe true
        parserNTC.parseAll("XYYZ".toList) shouldBe false
        parserNTC.parseAll("".toList) shouldBe true

    "Not Two consecutive and not empty parser" should "accept only not empty right strings without two consecutive chars" in:
        def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
        parserNTCNE.parseAll("XYZ".toList) shouldBe true
        parserNTCNE.parseAll("XYYZ".toList) shouldBe false
        parserNTCNE.parseAll("".toList) shouldBe false

    "String parser" should "accept only right strings" in:
        def sparser: Parser[Char] = "abc".charParser()
        sparser.parseAll("aabc".toList) shouldBe true
        sparser.parseAll("aabcdc".toList) shouldBe false
        sparser.parseAll("".toList) shouldBe true

    "Shorter then N parser" should "accept only strings with at most n chars" in:
        def parserSTN = new BasicParser(Set('a', 'b', 'c')) with ShortenThenN[Char](n = 5)
        parserSTN.parseAll("aabc".toList) shouldBe true
        parserSTN.parseAll("abdc".toList) shouldBe false
        parserSTN.parseAll("aabcac".toList) shouldBe false
        parserSTN.parseAll("".toList) shouldBe true