package ex1

import org.scalatest.matchers.should.Matchers.*
import ex1.Parsers.*

class ParserScalaTests extends org.scalatest.flatspec.AnyFlatSpec:
    def parser = new BasicParser(Set('a', 'b', 'c'))
    // Note NonEmpty being "stacked" on to a concrete class
    // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
    def parserNE = new NonEmptyParser(Set('0', '1'))
    def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    // note we do not need a class name here, we use the structural type
    def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    def sparser: Parser[Char] = "abc".charParser()

    "Basic parser" should "only have a, b and c" in:
        parser.parseAll("aabc".toList) shouldBe true
        parser.parseAll("aabcdc".toList) shouldBe false
        parser.parseAll("".toList) shouldBe true

    "Not Empty parser" should "be not empty and only have 0 and 1" in:
        parserNE.parseAll("0101".toList) shouldBe true
        parserNE.parseAll("0123".toList) shouldBe false
        parserNE.parseAll("".toList) shouldBe false

    "Not Two consecutive parser" should "only have X, Y and Z and not have two consecutive elements" in:
        parserNTC.parseAll("XYZ".toList) shouldBe true
        parserNTC.parseAll("XYYZ".toList) shouldBe false
        parserNTC.parseAll("".toList) shouldBe true

    "Not Two consecutive and not empty parser" should "only have X, Y and Z, not have two consecutive elements and be not empty" in:
        parserNTCNE.parseAll("XYZ".toList) shouldBe true
        parserNTCNE.parseAll("XYYZ".toList) shouldBe false
        parserNTCNE.parseAll("".toList) shouldBe false

    "String parser" should "only have a, b and c" in:
        sparser.parseAll("aabc".toList) shouldBe true
        sparser.parseAll("aabcdc".toList) shouldBe false
        sparser.parseAll("".toList) shouldBe true