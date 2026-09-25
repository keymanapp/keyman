//
//  MiscellaneousTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/21/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class KeymanHostTests: XCTestCase {
  /**
   * Ensures a test failure in case we accidentally leave `useLocal` enabled.
   */
  func testUseLocalDisabled() throws {
    XCTAssertFalse(KeymanHosts.useLocal)
  }

  /**
   * Ensures no accidental permanent edits to the .localhost variant URLs occur.
   */
  func testLocalSitesUnchanged() {
    XCTAssertEqual(KeymanHosts.getApiSiteURL(forTier: .stable, useLocal: true),
                   URL.init(string: "http://api.keyman.com.localhost"))
    XCTAssertEqual(KeymanHosts.getHelpSiteURL(forTier: .stable, useLocal: true),
                   URL.init(string: "http://help.keyman.com.localhost"))
    XCTAssertEqual(KeymanHosts.getMainSiteURL(forTier: .stable, useLocal: true),
                   URL.init(string: "http://keyman.com.localhost"))
  }
}
