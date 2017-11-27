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
  func testSectionAndProperty() throws {
    let string = """
      [section]
      key=value
    """
    let parser = INIParser()
    let actual = try parser.parse(string)
    let expected = ["section": ["key": "value"]]
    XCTAssertEqual(expected as NSObject, actual as NSObject)
  }
}
