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
  
  func testKeyboardInstallation() throws {
    let kmp = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP)
    XCTAssertNotNil(kmp, "Failed to prepare KMP for installation")
    XCTAssertNotNil(kmp as? KeyboardKeymanPackage, "KMP resource type improperly recognized - expected a keyboard package!")
    
    try ResourceFileManager.shared.finalizePackageInstall(kmp, isCustom: true)
    
    let installURL = Storage.active.keyboardURL(for: TestUtils.Keyboards.khmer_angkor)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: installURL.path),
                  "Could not find installed keyboard file")
    
    let keyboards = Storage.active.userDefaults.userKeyboards!
    
    XCTAssertEqual(keyboards.count, 1, "Unexpected number of keyboards were installed")
    XCTAssertEqual(keyboards[0].id, "khmer_angkor", "Installed keyboard ID mismatch")
    
    let fontURL = Storage.active.fontURL(forResource: TestUtils.Keyboards.khmer_angkor, filename: "Mondulkiri-R.ttf")!
    XCTAssertTrue(FileManager.default.fileExists(atPath: fontURL.path))
  }
  
  func testLexicalModelInstallation() throws {
    let kmp = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.mtntKMP)
    XCTAssertNotNil(kmp, "Failed to prepare KMP for installation")
    XCTAssertNotNil(kmp as? LexicalModelKeymanPackage, "KMP resource type improperly recognized - expected a lexical model package!")
    
    try ResourceFileManager.shared.finalizePackageInstall(kmp, isCustom: true)
    
    let installURL = Storage.active.lexicalModelURL(for: TestUtils.LexicalModels.mtnt)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: installURL.path),
                  "Could not find installed lexical model file")
    
    let models = Storage.active.userDefaults.userLexicalModels!
    
    // Yep, the model auto-installs for all language ids, even when there's no matching keyboard.
    // That's the current state of affairs in Keyman Engine for iOS.
    XCTAssertEqual(models.count, 3, "Unexpected number of models were installed")
    XCTAssertEqual(models[0].id, "nrc.en.mtnt", "Installed lexical model ID mismatch")
  }
  
  func testInstallKeyboardFromPackage() throws {
    // Standard installation
    let rawKMP = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP)
    XCTAssertNotNil(rawKMP, "Failed to prepare KMP for installation")
    guard let kmp = rawKMP as? KeyboardKeymanPackage else {
      XCTFail("KMP resource type improperly recognized - expected a keyboard package!")
      return
    }
    
    do {
      try ResourceFileManager.shared.install(resourceWithID: TestUtils.Keyboards.khmer_angkor.fullID, from: kmp)
    } catch {
      XCTFail("Unexpected error during KeyboardPackage install")
    }
    
    let installURL = Storage.active.keyboardURL(for: TestUtils.Keyboards.khmer_angkor)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: installURL.path),
                  "Could not find installed keyboard file")
    
    let keyboards = Storage.active.userDefaults.userKeyboards!
    
    XCTAssertEqual(keyboards.count, 1, "Unexpected number of keyboards were installed")
    XCTAssertEqual(keyboards[0].id, "khmer_angkor", "Installed keyboard ID mismatch")
    
    // While the LanguageResource definition we provided lacks font definitions, the
    // KMP's definition has that data.  By default, the KMP's definition takes precedence.
    let fontURL = Storage.active.fontURL(forResource: TestUtils.Keyboards.khmer_angkor, filename: "Mondulkiri-R.ttf")!
    XCTAssertTrue(FileManager.default.fileExists(atPath: fontURL.path))
  }
  
  func testInstallLexicalModelFromPackage() throws {
    let rawKMP = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.mtntKMP)
    XCTAssertNotNil(rawKMP, "Failed to prepare KMP for installation")
    guard let kmp = rawKMP as? LexicalModelKeymanPackage else {
      XCTFail("KMP resource type improperly recognized - expected a lexical model package!")
      return
    }
    
    do {
      try ResourceFileManager.shared.install(resourceWithID: TestUtils.LexicalModels.mtnt.fullID, from: kmp)
    } catch {
      XCTFail("Unexpected error during LexicalModelPackage install")
    }
    
    let installURL = Storage.active.lexicalModelURL(for: TestUtils.LexicalModels.mtnt)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: installURL.path),
                  "Could not find installed lexical model file")
    
    let models = Storage.active.userDefaults.userLexicalModels!
    
    // This variant is selective - only a single pairing should be installed for the model.
    XCTAssertEqual(models.count, 1, "Unexpected number of models were installed")
    XCTAssertEqual(models[0].id, "nrc.en.mtnt", "Installed lexical model ID mismatch")
  }
  
  func testInstallUpdateCheck() throws {
    // Has a resource in need of updates (sil_euro_latin)
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.cloud_to_kmp_13)
    
    let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as! KeyboardKeymanPackage
    let updatables = ResourceFileManager.shared.findPotentialUpdates(in: package)
    
    XCTAssertEqual(updatables.count, 2)
    XCTAssertTrue(updatables.contains(where: { $0.languageID == "en" }))
    XCTAssertTrue(updatables.contains(where: { $0.languageID == "fr" }))
  }
  
  func testGetInstalledPackageFor() throws {
    // Standard installation
    let rawKMP = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.khmerAngkorKMP)
    XCTAssertNotNil(rawKMP, "Failed to prepare KMP for installation")
    guard let kmp = rawKMP as? KeyboardKeymanPackage else {
      XCTFail("KMP resource type improperly recognized - expected a keyboard package!")
      return
    }
    
    do {
      try ResourceFileManager.shared.install(resourceWithID: TestUtils.Keyboards.khmer_angkor.fullID, from: kmp)
    } catch {
      XCTFail("Unexpected error during KeyboardPackage install")
    }
    
    // We did not install sil_euro_latin.
    XCTAssertNil(ResourceFileManager.shared.getInstalledPackage(for: TestUtils.Keyboards.sil_euro_latin))
    
    // We've installed khmer_angkor for 'km'.  Now to look up the resource from its InstallableKeyboard def.
    let km_package = ResourceFileManager.shared.getInstalledPackage(for: TestUtils.Keyboards.khmer_angkor)
    XCTAssertNotNil(km_package)
    if let package = km_package {
      XCTAssertFalse(package.metadata.isAutogeneratedWrapper)
      XCTAssertFalse(package.isTemp)
      let kbdMetadata: KMPKeyboard? = package.findMetadataMatchFor(resource: TestUtils.Keyboards.khmer_angkor, ignoreLanguage: false, ignoreVersion: false)
      XCTAssertNotNil(kbdMetadata)
    }
  }
  
  func testInstallStateForPackage() throws {
    // State 1:  Not installed, no download or install pending.
    let khmer_angkor_key = TestUtils.Keyboards.khmer_angkor.packageKey
    
    // No downloads have even started.
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: khmer_angkor_key), .none)
    
    // State 2:  Download requested.
    //           Note - we don't need to actually run the queue.
    let downloadManager = ResourceDownloadManager(session: TestUtils.Downloading.URLSessionMock(), autoExecute: false)
    downloadManager.downloadPackage(withKey: khmer_angkor_key,
                                    from: TestUtils.Keyboards.khmerAngkorKMP,
                                    completionBlock: { _, _ in })
    
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: khmer_angkor_key, withManager: downloadManager), .downloading)
    
    // State 3:  During install process.
    
    // If it was just downloaded, that means it should exist within the Documents directory.
    let documentsKMPURL = ResourceFileManager.shared.importFile(TestUtils.Keyboards.khmerAngkorKMP)!
    guard let installPackage = try ResourceFileManager.shared.prepareKMPInstall(from: documentsKMPURL) as? KeyboardKeymanPackage else {
      XCTFail("Could not load keyboard KMP for test")
      return
    }
    
    XCTAssertEqual(installPackage.installState, .pending)
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: khmer_angkor_key), .pending)
    
    // State 4:  package is fully installed.
    try ResourceFileManager.shared.install(resourceWithID: TestUtils.Keyboards.khmer_angkor.fullID, from: installPackage)
    
    // Retrieve an instance for the installation.  We're currently using the temp version.
    guard let package = ResourceFileManager.shared.getInstalledPackage(withKey: installPackage.key) else {
      XCTFail("Could not load installed form of keyboard KMP for test")
      return
    }
    
    XCTAssertEqual(installPackage.installState, .pending) // Reflects that the instance is a temporary extraction.
    XCTAssertEqual(package.installState, .installed) // Reflects the actually-installed package
    XCTAssertEqual(ResourceFileManager.shared.installState(forPackage: khmer_angkor_key), .installed)
  }
}
