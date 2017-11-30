//
//  INIParserTests.swift
//  KeymanEngineTests
//
//  Created by Gabriel Wong on 2017-11-27.
//  Copyright © 2017 SIL International. All rights reserved.
//

import XCTest

@testable import KeymanEngine

class INIParserTests: XCTestCase {
  var parser: INIParser!

  override func setUp() {
    parser = INIParser()
  }

  func testSingleSectionAndProperty() throws {
    let string = """
      [section]
      key=value
    """
    let actual = try parser.parse(string)
    let expected = ["section": ["key": "value"]]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testMultipleSectionsAndProperties() throws {
    let string = """
      [foo]
      key=value
      foo=bar
      [bar]
      bar=baz
      baz=foo
    """
    let actual = try parser.parse(string)
    let expected = [
      "foo": ["key": "value",
              "foo": "bar"],
      "bar": ["bar": "baz",
              "baz": "foo"]
    ]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testPropertyWithoutSection() throws {
    let string = """
      foo=bar
      [section]
      key=value
    """
    let actual = try parser.parse(string)
    let expected = [
      "": ["foo": "bar"],
      "section": ["key": "value"]
    ]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testSectionWithoutProperty() throws {
    let string = """
      [section]
      key=value
      [empty section]
      [another empty section]
    """
    let actual = try parser.parse(string)
    let expected = [
      "section": ["key": "value"],
      "empty section": [:],
      "another empty section": [:]
    ]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testMultipleEqualSignsInProperty() throws {
    let string = """
      [section]
      this=is=valid
    """
    let actual = try parser.parse(string)
    let expected = [
      "section": ["this": "is=valid"],
    ]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testQuotationMarksAndCommas() throws {
    let string = """
      [section]
      key="some list",of,"values,"
    """
    let actual = try parser.parse(string)
    let expected = [
      "section": ["key": "\"some list\",of,\"values,\""],
      ]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testCommentsAndWhitespace() throws {
    let string = """
      ; blank lines and lines containing only whitespace are removed
          \t

          \t ; leading and trailing whitespace is stripped from all lines
         [section] \t
           strip=leading whitespace
      also strip=trailing whitespace   \t
      don't strip   =   around the equal sign
    """
    let actual = try parser.parse(string)
    let expected = [
      "section": ["strip": "leading whitespace",
                  "also strip": "trailing whitespace",
                  "don't strip   ": "   around the equal sign"]
    ]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testEmpty() throws {
    let actual = try parser.parse("")
    let expected: [String: [String: String]] = [:]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testEmptyProperty() throws {
    let actual = try parser.parse("foo=")
    let expected = ["": ["foo": ""]]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }

  func testCharactersAfterSectionHeader() {
    XCTAssertThrowsError(try parser.parse("[section]invalid"))
    XCTAssertThrowsError(try parser.parse("[section]]"))
    XCTAssertThrowsError(try parser.parse("[section]foo]"))
  }

  func testDuplicateKeyNames() {
    let string = """
      [section]
      foo=bar
      bar=baz
      foo=bar
    """
    XCTAssertThrowsError(try parser.parse(string))
  }

  func testDuplicateConsecutiveKeyNames() {
    let string = """
      [section]
      foo=bar
      foo=bar
    """
    XCTAssertThrowsError(try parser.parse(string))
  }

  func testDuplicateSectionNames() {
    let string = """
      [foo]
      [bar]
      [foo]
    """
    XCTAssertThrowsError(try parser.parse(string))
  }

  func testConsecutiveDuplicateSectionNames() {
    let string = """
      [section]
      [section]
    """
    XCTAssertThrowsError(try parser.parse(string))
  }

  func testMissingSectionClosingBracket() {
    XCTAssertThrowsError(try parser.parse("[invalid"))
  }

  func testMissingEqualSignInProperty() {
    let string = """
      [section]
      noequalsign
    """
    XCTAssertThrowsError(try parser.parse(string))
  }

  func testUnquoteUnquotedString() throws {
    XCTAssertEqual("foo", try parser.unquotedString("foo"))
  }

  func testUnquoteQuotedString() throws {
    XCTAssertEqual("foo", try parser.unquotedString("\"foo\""))
  }

  func testUnquoteWithoutClosingQuote() {
    XCTAssertThrowsError(try parser.unquotedString("\"foo"))
    XCTAssertThrowsError(try parser.unquotedString("\""))
  }

  func testUnquoteWithMultipleQuotes() {
    XCTAssertThrowsError(try parser.unquotedString("\"foo\"\""))
    XCTAssertThrowsError(try parser.unquotedString("\"\"foo\"\""))
    XCTAssertThrowsError(try parser.unquotedString("\"foo\"bar\""))
    XCTAssertThrowsError(try parser.unquotedString("\"foo\"\"bar\""))
    XCTAssertThrowsError(try parser.unquotedString("\"foo\"baz\"bar\""))
  }

  func testUnquoteWithCharactersBeforeQuote() {
    XCTAssertThrowsError(try parser.unquotedString("foo\"bar\""))
    XCTAssertThrowsError(try parser.unquotedString("foo\""))
  }

  func testUnquoteWithCharactersAfterQuote() {
    XCTAssertThrowsError(try parser.unquotedString("\"foo\"bar"))
  }

  func testComponents() throws {
    XCTAssertEqual(["foo"], try parser.components("foo"))
    XCTAssertEqual(["foo"], try parser.components("\"foo\""))
    XCTAssertEqual(["foo", "bar"], try parser.components("foo,bar"))
    XCTAssertEqual(["foo,bar"], try parser.components("\"foo,bar\""))
    XCTAssertEqual(["foo", "bar"], try parser.components("\"foo\",bar"))
    XCTAssertEqual(["foo", "bar"], try parser.components("foo,\"bar\""))
  }

  func testEmptyComponents() throws {
    XCTAssertEqual([""], try parser.components(""))
    XCTAssertEqual([""], try parser.components("\"\""))
    XCTAssertEqual(["", ""], try parser.components(","))
    XCTAssertEqual(["", ""], try parser.components("\"\","))
    XCTAssertEqual(["", ""], try parser.components(",\"\""))
    XCTAssertEqual(["foo", "", "bar"], try parser.components("foo,,bar"))
    XCTAssertEqual(["foo", ""], try parser.components("foo,"))
    XCTAssertEqual(["", "foo"], try parser.components(",foo"))
  }

  func testComponentsWithUnterminatedQuote() {
    XCTAssertThrowsError(try parser.components("\""))
    XCTAssertThrowsError(try parser.components("\"foo"))
    XCTAssertThrowsError(try parser.components("\"foo,"))
    XCTAssertThrowsError(try parser.components("\"foo\",\""))
  }

  func testComponentsWithTextAdjacentToQuote() {
    XCTAssertThrowsError(try parser.components("foo\"bar\""))
    XCTAssertThrowsError(try parser.components("\"foo\"bar"))
    XCTAssertThrowsError(try parser.components("foo\"bar\"baz"))
  }
}
