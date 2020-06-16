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

  func testVersion13CloudToKMPMigration() throws {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.cloud_to_kmp_13)

    let initialUserKeyboards = Storage.active.userDefaults.userKeyboards!
    let initialUserModels = Storage.active.userDefaults.userLexicalModels!

    // A baseline load-check; confirms we have the correct resource count at the start.
    XCTAssertEqual(initialUserKeyboards.count, 7)
    XCTAssertEqual(initialUserModels.count, 6)

    // TODO:  Actual migration unit testing.  So far, this is really more of a
    //        TestUtils.Migrations.applyBundleToFileSystem unit test.
    try Migrations.migrateCloudResourcesToKMPFormat()

    // Now that migration has completed, it's time to check the results.

    let userKeyboards = Storage.active.userDefaults.userKeyboards!
    let userModels = Storage.active.userDefaults.userLexicalModels!

    // Pass 1:  do we have a matching set of resources after the migration?
    XCTAssertEqual(userKeyboards.count, 7)
    XCTAssertEqual(userModels.count, 6)

    userKeyboards.forEach{ kbd in
      XCTAssertTrue(initialUserKeyboards.contains(where: { $0.fullID == kbd.fullID }))
      XCTAssertEqual(kbd.packageID, kbd.id)
    }

    userModels.forEach{ lm in
      XCTAssertTrue(initialUserModels.contains(where: { $0.fullID == lm.fullID }))
      XCTAssertEqual(lm.packageID, lm.id)
    }

    // TODO:  If sil_sahu has actually-extracted KMP contents
    // TODO:  If the synthetic metadata flag is correct for all entries
        // TODO:  And version is set appropriately based on flag.
    // TODO:  If sil_eurolatin has two language members, others have one
  }

  func testVersion12ResourceMigration() {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.simple_12)
    Migrations.updateResources(storage: Storage.active)

    let userDefaults = Storage.active.userDefaults

    // The two keyboards should remain in the same location, while the lexical model should be upgraded.
    // v 0.1.2 -> 0.1.4.
    // Minor surprise (as of 13.0) is that 0.1.2's file actually remains!

    let defaultModel = userDefaults.userLexicalModels![0]
    XCTAssertEqual(defaultModel.id, Defaults.lexicalModel.id)
    XCTAssertEqual(defaultModel.version, Defaults.lexicalModel.version)
    let modelURL = Storage.active.lexicalModelURL(for: TestUtils.LexicalModels.mtnt) // The bundled, 0.1.4 version.
    XCTAssert(FileManager.default.fileExists(atPath: modelURL.path))
  }

  func testVersion12AdhocMigration() throws {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.adhoc_12)
    Storage.active.userDefaults.lastEngineVersion = Version.firstTracked
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

  func testVersion10ResourceMigration() {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.simple_10)
    Storage.active.userDefaults.lastEngineVersion = nil
    Migrations.updateResources(storage: Storage.active)

    let userDefaults = Storage.active.userDefaults

    // The old keyboard should be gone entirely, replaced by the current version's default resources.
    // There should only be one default resource per type (at least, as of 13.0.)
    XCTAssertEqual(userDefaults.userKeyboards!.count, 1, "Unexpected keyboard count after migration!")
    // 3, b/c all language-code variants are counted.
    XCTAssertEqual(userDefaults.userLexicalModels!.count, 3, "Unexpected lexical model count after migration!")

    let defaultKeyboard = userDefaults.userKeyboards![0]
    XCTAssertNotEqual(TestUtils.Migrations.european2.id, defaultKeyboard.id) // Double-ensure that the ID is new.
    XCTAssertEqual(defaultKeyboard.id, Defaults.keyboard.id)
    XCTAssertEqual(defaultKeyboard.version, Defaults.keyboard.version)

    let defaultModel = userDefaults.userLexicalModels![0]
    XCTAssertEqual(defaultModel.id, Defaults.lexicalModel.id)
    XCTAssertEqual(defaultModel.version, Defaults.lexicalModel.version)

    let keyboardURL = Storage.active.keyboardURL(for: Defaults.keyboard)
    let modelURL = Storage.active.lexicalModelURL(for: Defaults.lexicalModel)
    XCTAssert(FileManager.default.fileExists(atPath: keyboardURL.path))
    XCTAssert(FileManager.default.fileExists(atPath: modelURL.path))
  }

  func testNoDefaultVersion10ResourceMigration() {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.noDefault_10)
    Migrations.updateResources(storage: Storage.active)

    Storage.active.userDefaults.lastEngineVersion = nil
    let userDefaults = Storage.active.userDefaults

    // No new resources should be installed - only what was originally present should be there.
    XCTAssertEqual(userDefaults.userKeyboards!.count, 1, "Unexpected keyboard count after migration!")
    // There was no prior lexical model installed, so the installation still proceeds.  The user just won't see it.
    XCTAssertNil(userDefaults.userLexicalModels, "Unexpected lexical model(s) installed after migration!")

    let keyboard = userDefaults.userKeyboards![0]
    XCTAssertEqual(keyboard.id, TestUtils.Keyboards.khmer_angkor.id, "Unexpected keyboard was installed after migration!")
    XCTAssertEqual(keyboard.version, TestUtils.Keyboards.khmer_angkor.version)

    // If successful, confirms that the current default resources are not installed - as expected.
  }
}
