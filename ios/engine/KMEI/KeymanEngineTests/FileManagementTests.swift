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

    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    TestUtils.clearDirectory(at: cacheDirectory)
  }

  // AMDD acceptance test for existing KeymanPackage so I'll know what has to keep working despite my changers
  func testKeyboardPackageExtraction() throws {
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let khmerPackageZip = cacheDirectory.appendingPathComponent("khmer_angkor.zip")
    try FileManager.default.copyItem(at: TestUtils.Keyboards.khmerAngkorKMP, to: khmerPackageZip)

    let destinationFolderURL = cacheDirectory.appendingPathComponent("khmer_angkor")

    // Requires that the source file is already .zip, not .kmp.  It's a ZipUtils limitation.
    do {
      try KeymanPackage.extract(fileUrl: khmerPackageZip, destination: destinationFolderURL, complete: { kmp in
        if let kmp = kmp {
          XCTAssertTrue(kmp.isKeyboard(), "Keyboard KMP test extraction did not yield a keyboard package!")

          // extracted ok, test kmp
          XCTAssert(kmp.sourceFolder == destinationFolderURL,
                    "The KMP's reported 'source folder' should match the specified destination folder")
        } else {
          XCTAssert(false, "KeymanPackage.extract failed")
        }
      }) // Use XCTAssert and related functions to verify your tests produce the correct results.
    } catch {
      XCTFail("KeymanPackage.extract failed with error \(error)")
    }
  }

  // AMDD acceptance test for existing KeymanPackage so I'll know what has to keep working despite my changers
  func testLexicalModelPackageExtraction() throws {
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    let mtntZip = cacheDirectory.appendingPathComponent("mtnt.zip")
    try FileManager.default.copyItem(at: TestUtils.LexicalModels.mtntKMP, to: mtntZip)

    let destinationFolderURL = cacheDirectory.appendingPathComponent("mtnt.model")

    // Requires that the source file is already .zip, not .kmp.  It's a ZipUtils limitation.
    do {
      try KeymanPackage.extract(fileUrl: mtntZip, destination: destinationFolderURL, complete: { kmp in
        if let kmp = kmp {
          XCTAssertTrue(!kmp.isKeyboard(), "Lexical model KMP test extraction yielded a keyboard package!")

          // extracted ok, test kmp
          XCTAssert(kmp.sourceFolder == destinationFolderURL,
                    "The KMP's reported 'source folder' should match the specified destination folder")
        } else {
          XCTAssert(false, "KeymanPackage.extract failed")
        }
      }) // Use XCTAssert and related functions to verify your tests produce the correct results.
    } catch {
      XCTFail("KeymanPackage.extract failed with error \(error)")
    }
  }
}
