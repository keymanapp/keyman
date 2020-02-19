//
//  TestUtils.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/19/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest

import Foundation
@testable import KeymanEngine

/**
 * This enum will 'namespace' the various testing utility functions used for our unit tests.
 * It's a common Swift pattern, used because enums can't be instantiated.
 */
enum TestUtils {
  static let mainBundle = Bundle(for: FileManagementTests.self)  // Can't use TestUtils, as it's an enum

  private static func findSubBundle(forResource resource: String, ofType type: String) -> Bundle {
    return Bundle(path: mainBundle.path(forResource: resource, ofType: type)!)!
  }

  static let keyboardsBundle = findSubBundle(forResource: "Keyboards", ofType: "bundle")
  static let lexicalModelsBundle = findSubBundle(forResource: "Lexical Models", ofType: "bundle")

  static func clearDirectory(at url: URL) {
    do {
      // We'll run into problems if we delete the cache directory.  Instead, we delete all the items within it.
      let itemsInDirectory = try FileManager.default.contentsOfDirectory(at: url, includingPropertiesForKeys: nil)
      try itemsInDirectory.forEach { item in
        try FileManager.default.removeItem(at: item)
      }
    } catch {
      log.error(error)
      XCTFail("Could not clear a directory because of error: \(error)")
    }
  }

  // Not the greatest thing, but this cleanup method is "white-box" - make sure it's up to date with Storage's init!
  static func eraseStorage(_ storage: Storage) {
    clearDirectory(at: storage.baseDir)

    do {
      try FileManager.default.createDirectory(at: storage.keyboardDir,
                                              withIntermediateDirectories: true,
                                              attributes: nil)
      try FileManager.default.createDirectory(at: storage.lexicalModelDir,
                                              withIntermediateDirectories: true,
                                              attributes: nil)
    } catch {
      log.error(error)
      XCTFail("Could not properly erase & reinit KeymanEngine Storage because of error: \(error)")
    }
  }
}
