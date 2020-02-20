//
//  MigrationTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/20/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
@testable import KeymanEngine
import XCTest

class MigrationTests: XCTestCase {
  override func tearDown() {
    TestUtils.standardTearDown()
  }

  func testVersion12ResourceMigration() {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.simple_12)

    let userDefaults = Storage.active.userDefaults
    userDefaults.lastEngineVersion = Version("12.0")!

    let versionResources = TestUtils.Migrations.getVersionHistory(for: Version("12.0")!)
    userDefaults.userKeyboards?.append(versionResources[0] as! InstallableKeyboard)
    userDefaults.userKeyboards?.append(TestUtils.Keyboards.khmer_angkor)
    userDefaults.userLexicalModels?.append(versionResources[1] as! InstallableLexicalModel)

    Migrations.updateResources(storage: Storage.active)

    // The two keyboards should remain in the same location, while the lexical model should be upgraded.
    // v 0.1.2 -> 0.1.4.
    // Minor surprise (as of 13.0) is that 0.1.2's file actually remains!

    let mtnt = userDefaults.userLexicalModels?[0]
    XCTAssertEqual(mtnt?.version, "0.1.4")
    let modelURL = Storage.active.lexicalModelURL(for: TestUtils.LexicalModels.mtnt) // The bundled, 0.1.4 version.
    XCTAssert(FileManager.default.fileExists(atPath: modelURL.path))
  }

  func testVersion12AdhocMigration() throws {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.adhoc_12)

    let userDefaults = Storage.active.userDefaults
    userDefaults.lastEngineVersion = Version("12.0")!
    userDefaults.userKeyboards?.append(TestUtils.Keyboards.khmer10)

    Migrations.migrate(storage: Storage.active)

    // The files in the .documents directory should be erased after this method is run.

    let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    let documentsContents = try FileManager.default.contentsOfDirectory(atPath: documentsDirectory.path)
    XCTAssertEqual(documentsContents.count, 1, "Documents directory was not purged of installation by-products during ad-hoc resource migration!")

    // The .kmp.zip is converted back into its original .kmp file, which remains in the Documents directory
    // so that it may be used for sharing.
    let kmpURL = URL(fileURLWithPath: documentsContents[0])
    XCTAssertEqual(kmpURL.lastPathComponent, "khmer10.kmp")
  }
}
