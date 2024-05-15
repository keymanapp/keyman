//
//  TestUtils.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/19/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
import Foundation
import os.log

@testable import KeymanEngine

/**
 * This enum will 'namespace' the various testing utility functions used for our unit tests.
 * It's a common Swift pattern, used because enums can't be instantiated.
 */
enum TestUtils {
  // Sets up an internally-nested "namespace".
  enum Downloading {
    
  }
  
  static let mainBundle = Bundle(for: FileManagementTests.self)  // Can't use TestUtils, as it's an enum
  
  static func findSubBundle(forResource resource: String, ofType type: String) -> Bundle {
    return Bundle(path: mainBundle.path(forResource: resource, ofType: type)!)!
  }
  
  static let keyboardsBundle = findSubBundle(forResource: "Keyboards", ofType: "bundle")
  static let lexicalModelsBundle = findSubBundle(forResource: "Lexical Models", ofType: "bundle")
  
  static let mockedError = NSError(domain: "KeymanTests", code: 1, userInfo: nil)
  
  static func clearDirectory(at url: URL) {
    do {
      // We'll run into problems if we delete the cache directory.  Instead, we delete all the items within it.
      let itemsInDirectory = try FileManager.default.contentsOfDirectory(at: url, includingPropertiesForKeys: nil)
      try itemsInDirectory.forEach { item in
        try FileManager.default.removeItem(at: item)
      }
    } catch {
      let message = "Could not clear a directory because of error: \(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .debug, message)
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
      let message = "Could not properly erase & reinit KeymanEngine Storage because of error: \(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .debug, message)
      XCTFail("Could not properly erase & reinit KeymanEngine Storage because of error: \(error)")
    }
  }
  
  static func setupAndDeinitManager() {
    // Ensures that Manager has been initialized...
    _ = Manager.shared
    
    // and then "de-init" the file system and defaults changes that the init triggers.
    TestUtils.eraseStorage(Storage.active)
    TestUtils.UserDefaults.clear(from: Storage.active)
  }
  
  static func standardTearDown() {
    if let storage = Storage.active {
      TestUtils.eraseStorage(storage)
      TestUtils.UserDefaults.clear(from: storage)
    }
    
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    TestUtils.clearDirectory(at: cacheDirectory)
    TestUtils.clearDirectory(at: documentsDirectory)
  }
}
