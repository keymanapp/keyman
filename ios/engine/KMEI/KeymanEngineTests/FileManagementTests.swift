//
//  KeymanEngineTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2020-02-19.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest

@testable import KeymanEngine
import DeviceKit


class FileManagementTests: XCTestCase {
  override func setUp() {
    TestUtils.setupAndDeinitManager()
  }
  
  override func tearDown() {
    TestUtils.standardTearDown()
  }

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
      })
    } catch {
      XCTFail("KeymanPackage.extract failed with error \(error)")
    }
  }

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
      })
    } catch {
      XCTFail("KeymanPackage.extract failed with error \(error)")
    }
  }

  func testKeyboardInstallation() {
    ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP) { kmp, error in
      XCTAssertNotNil(kmp, "Failed to prepare KMP for installation")
      XCTAssertNil(error, "Error occurred while preparing KMP for installation")
      XCTAssertTrue(kmp!.isKeyboard(), "KMP resource type improperly recognized - expected a keyboard package!")

      ResourceFileManager.shared.finalizePackageInstall(kmp!, isCustom: true) { innerError in
        XCTAssertNil(innerError, "Error occurred while finalizing KMP installation")

        let installURL = Storage.active.keyboardURL(forID: "khmer_angkor", version: "1.0.6")

        XCTAssertTrue(FileManager.default.fileExists(atPath: installURL.path),
                      "Could not find installed keyboard file")

        let keyboards = Storage.active.userDefaults.userKeyboards!

        XCTAssertEqual(keyboards.count, 1, "Unexpected number of keyboards were installed")
        XCTAssertEqual(keyboards[0].id, "khmer_angkor", "Installed keyboard ID mismatch")
      }
    }
  }

  func testLexicalModelInstallation() {
    ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.mtntKMP) { kmp, error in
      XCTAssertNotNil(kmp, "Failed to prepare KMP for installation")
      XCTAssertNil(error, "Error occurred while preparing KMP for installation")
      XCTAssertFalse(kmp!.isKeyboard(), "KMP resource type improperly recognized - expected a lexical model package!")

      ResourceFileManager.shared.finalizePackageInstall(kmp!, isCustom: true) { innerError in
        XCTAssertNil(innerError, "Error occurred while finalizing KMP installation")

        let installURL = Storage.active.lexicalModelURL(forID: "nrc.en.mtnt", version: "0.1.4")

        XCTAssertTrue(FileManager.default.fileExists(atPath: installURL.path),
                      "Could not find installed lexical model file")

        let models = Storage.active.userDefaults.userLexicalModels!

        // Yep, the model auto-installs for all language ids, even when there's no matching keyboard.
        // That's the current state of affairs in Keyman Engine for iOS.
        XCTAssertEqual(models.count, 3, "Unexpected number of models were installed")
        XCTAssertEqual(models[0].id, "nrc.en.mtnt", "Installed lexical model ID mismatch")
      }
    }
  }
}
