//
//  ResourceDownloadQueueTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/29/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class ResourceDownloadQueueTests: XCTestCase {
  override func tearDown() {
    // Resets resource directories for a clean slate.
    TestUtils.standardTearDown()
  }

  func testResourceDownloadFinalizeClosureNoOverwrite() throws {
    let khmer_angkor_src_url = TestUtils.Keyboards.khmerAngkorKMP
    let tempFileURL = ResourceFileManager.shared.packageDownloadTempPath(forID: TestUtils.Keyboards.khmer_angkor.fullID)
    let destFileURL = ResourceFileManager.shared.cachedPackagePath(forID: TestUtils.Keyboards.khmer_angkor.fullID)

    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: tempFileURL)

    XCTAssertTrue(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertFalse(FileManager.default.fileExists(atPath: destFileURL.path))

    let closure = DownloadBatch<FullKeyboardID>.resourceDownloadFinalizeClosure(tempURL: tempFileURL, finalURL: destFileURL) { package, error in
      XCTAssertNotNil(package)
      XCTAssertNil(error)
    }

    // May need a stand-in empty package for the first parameter in the future.
    let khmer_angkor_metadata = KMPMetadata(from: TestUtils.Keyboards.khmer_angkor)
    // Creates a placeholder package, rather than extracting a real one for use as a placeholder.
    let dummyPackage = KeyboardKeymanPackage(metadata: khmer_angkor_metadata, folder: TestUtils.keyboardsBundle.bundleURL)
    closure(dummyPackage, nil)

    XCTAssertFalse(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))
  }

  func testResourceDownloadFinalizeClosureWithOverwrite() throws {
    let khmer_angkor_src_url = TestUtils.Keyboards.khmerAngkorKMP
    let tempFileURL = ResourceFileManager.shared.packageDownloadTempPath(forID: TestUtils.Keyboards.khmer_angkor.fullID)
    let destFileURL = ResourceFileManager.shared.cachedPackagePath(forID: TestUtils.Keyboards.khmer_angkor.fullID)

    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: tempFileURL)
    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: destFileURL)

    XCTAssertTrue(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))

    let closure = DownloadBatch<FullKeyboardID>.resourceDownloadFinalizeClosure(tempURL: tempFileURL, finalURL: destFileURL) { package, error in
      XCTAssertNotNil(package)
      XCTAssertNil(error)
    }

    // May need a stand-in empty package for the first parameter in the future.
    let khmer_angkor_metadata = KMPMetadata(from: TestUtils.Keyboards.khmer_angkor)
    // Creates a placeholder package, rather than extracting a real one for use as a placeholder.
    let dummyPackage = KeyboardKeymanPackage(metadata: khmer_angkor_metadata, folder: TestUtils.keyboardsBundle.bundleURL)
    closure(dummyPackage, nil)

    XCTAssertFalse(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))
  }

  func testResourceDownloadFinalizeClosureWithError() throws {
    let khmer_angkor_src_url = TestUtils.Keyboards.khmerAngkorKMP
    let tempFileURL = ResourceFileManager.shared.packageDownloadTempPath(forID: TestUtils.Keyboards.khmer_angkor.fullID)
    let destFileURL = ResourceFileManager.shared.cachedPackagePath(forID: TestUtils.Keyboards.khmer_angkor.fullID)

    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: tempFileURL)

    XCTAssertTrue(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertFalse(FileManager.default.fileExists(atPath: destFileURL.path))

    let closure = DownloadBatch<FullKeyboardID>.resourceDownloadFinalizeClosure(tempURL: tempFileURL, finalURL: destFileURL) { package, error in
      XCTAssertNil(package)
      XCTAssertNotNil(error)
    }

    // Error chosen as a stand-in / dummy value.
    closure(nil, ResourceDownloadQueue.QueueState.busy.error)

    XCTAssertFalse(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertFalse(FileManager.default.fileExists(atPath: destFileURL.path))
  }

  func testResourceDownloadFinalizeClosurePreexistingWithError() throws {
    let khmer_angkor_src_url = TestUtils.Keyboards.khmerAngkorKMP
    let tempFileURL = ResourceFileManager.shared.packageDownloadTempPath(forID: TestUtils.Keyboards.khmer_angkor.fullID)
    let destFileURL = ResourceFileManager.shared.cachedPackagePath(forID: TestUtils.Keyboards.khmer_angkor.fullID)

    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: tempFileURL)
    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: destFileURL)

    XCTAssertTrue(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))

    let closure = DownloadBatch<FullKeyboardID>.resourceDownloadFinalizeClosure(tempURL: tempFileURL, finalURL: destFileURL) { package, error in
      XCTAssertNil(package)
      XCTAssertNotNil(error)
    }

    // Error chosen as a stand-in / dummy value.
    closure(nil, ResourceDownloadQueue.QueueState.busy.error)

    XCTAssertFalse(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))
  }
}
