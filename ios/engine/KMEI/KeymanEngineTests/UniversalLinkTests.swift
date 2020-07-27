//
//  UniversalLinkTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/27/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class UniversalLinkTests: XCTestCase {
  func testTryLinkParse() {
    var parsedLink = UniversalLinks.tryParseKeyboardInstallLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/randomURL")!)

    XCTAssertNil(parsedLink)

    parsedLink = UniversalLinks.tryParseKeyboardInstallLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/keyboards/install/khmer_angkor")!)

    if let parsedLink = parsedLink {
      XCTAssertEqual(parsedLink.keyboard_id, "khmer_angkor")
      XCTAssertNil(parsedLink.lang_id)
    }

    parsedLink = UniversalLinks.tryParseKeyboardInstallLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/keyboards/install/khmer_angkor?bcp47=km")!)

    if let parsedLink = parsedLink {
      XCTAssertEqual(parsedLink.keyboard_id, "khmer_angkor")
      XCTAssertEqual(parsedLink.lang_id, "km")
    }

    parsedLink = UniversalLinks.tryParseKeyboardInstallLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/keyboards/install/sil_euro_latin")!)

    if let parsedLink = parsedLink {
      XCTAssertEqual(parsedLink.keyboard_id, "sil_euro_latin")
      XCTAssertNil(parsedLink.lang_id)
    }

    parsedLink = UniversalLinks.tryParseKeyboardInstallLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/keyboards/install/foo?bcp47=bar")!)

    if let parsedLink = parsedLink {
      XCTAssertEqual(parsedLink.keyboard_id, "foo")
      XCTAssertEqual(parsedLink.lang_id, "bar")
    }
  }
}
