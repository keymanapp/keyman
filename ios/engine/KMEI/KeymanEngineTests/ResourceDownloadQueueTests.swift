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
    let key = TestUtils.Keyboards.khmer_angkor.packageKey
    
    let tempFileURL = ResourceFileManager.shared.packageDownloadTempPath(forKey: key)
    let destFileURL = ResourceFileManager.shared.cachedPackagePath(forKey: key)
    
    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: tempFileURL)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertFalse(FileManager.default.fileExists(atPath: destFileURL.path))
    
    let closure = DownloadTask.resourceDownloadFinalizationClosure(tempURL: tempFileURL, finalURL: destFileURL)
    try? closure(true)
    
    XCTAssertFalse(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))
  }
  
  func testResourceDownloadFinalizeClosureWithOverwrite() throws {
    let khmer_angkor_src_url = TestUtils.Keyboards.khmerAngkorKMP
    let key = TestUtils.Keyboards.khmer_angkor.packageKey
    
    let tempFileURL = ResourceFileManager.shared.packageDownloadTempPath(forKey: key)
    let destFileURL = ResourceFileManager.shared.cachedPackagePath(forKey: key)
    
    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: tempFileURL)
    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: destFileURL)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))
    
    let closure = DownloadTask.resourceDownloadFinalizationClosure(tempURL: tempFileURL, finalURL: destFileURL)
    
    // May need a stand-in empty package for the first parameter in the future.
    try? closure(true)
    
    XCTAssertFalse(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))
  }
  
  func testResourceDownloadFinalizeClosureWithError() throws {
    let khmer_angkor_src_url = TestUtils.Keyboards.khmerAngkorKMP
    let key = TestUtils.Keyboards.khmer_angkor.packageKey
    
    let tempFileURL = ResourceFileManager.shared.packageDownloadTempPath(forKey: key)
    let destFileURL = ResourceFileManager.shared.cachedPackagePath(forKey: key)
    
    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: tempFileURL)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertFalse(FileManager.default.fileExists(atPath: destFileURL.path))
    
    let closure = DownloadTask.resourceDownloadFinalizationClosure(tempURL: tempFileURL, finalURL: destFileURL)
    
    // Error occurred; abort.
    try? closure(false)
    
    XCTAssertFalse(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertFalse(FileManager.default.fileExists(atPath: destFileURL.path))
  }
  
  func testResourceDownloadFinalizeClosurePreexistingWithError() throws {
    let khmer_angkor_src_url = TestUtils.Keyboards.khmerAngkorKMP
    let key = TestUtils.Keyboards.khmer_angkor.packageKey
    
    let tempFileURL = ResourceFileManager.shared.packageDownloadTempPath(forKey: key)
    let destFileURL = ResourceFileManager.shared.cachedPackagePath(forKey: key)
    
    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: tempFileURL)
    try FileManager.default.copyItem(at: khmer_angkor_src_url, to: destFileURL)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))
    
    let closure = DownloadTask.resourceDownloadFinalizationClosure(tempURL: tempFileURL, finalURL: destFileURL)
    
    // Error occurred, abort.
    try? closure(false)
    
    XCTAssertFalse(FileManager.default.fileExists(atPath: tempFileURL.path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: destFileURL.path))
  }
}
