//
//  KeymanEngineTests.swift
//  KeymanEngineTests
//
//  Created by Randy Boring on 3/7/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import XCTest

@testable import KeymanEngine
import DeviceKit


class FileManagementTests: XCTestCase {
  override func setUp() {
    // Put setup code here. This method is called before the invocation of each test method in the class.
  }
  
  override func tearDown() {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    if let storage = Storage.active {
      TestUtils.eraseStorage(storage)
      TestUtils.UserDefaults.clearUserDefaults(from: storage)
    }
  }

  // AMDD acceptance test for existing KeymanPackage so I'll know what has to keep working despite my changers
  func testExample() {
    let testUrl = URL(fileURLWithPath: "/Users/Shared/testpackage.kmp");
    let outUrl  = URL(fileURLWithPath: "/Users/Shared/outPackageFolder")

    // Note:  does not detect failure to extract!  This is basically leftover material from a previous dev's
    //        original attempt to add unit testing.
    //
    //        Honestly, that's the only reason this passes.  This test isn't actually functional yet.
    KeymanPackage.extract(fileUrl: testUrl, destination: outUrl, complete: { kmp in
      if let kmp = kmp {
        // extracted ok, test kmp
        XCTAssert(kmp.sourceFolder == outUrl, "the sourceFolder should be outURL")
      } else {
        XCTAssert(false, "KeymanPackage.extract failed")
      }
    }) // Use XCTAssert and related functions to verify your tests produce the correct results.
  }
}
