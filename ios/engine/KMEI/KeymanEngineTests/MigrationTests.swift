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
  
  //  // Uncomment this and set up whatever resources are needed for your test bundle in order to build one!
  //  func testForMigrationBundleConstruction() {
  //    do {
  //      if let sil_euro_latin = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.silEuroLatinKMP) as? KeyboardKeymanPackage {
  //      try ResourceFileManager.shared.install(resourceWithID: TestUtils.Keyboards.sil_euro_latin.fullID, from: sil_euro_latin)
  //      } else {
  //          XCTFail()
  //      }
  //
  //      if let sencoten_kbd_kmp = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.Keyboards.fvSencotenKMP) as? KeyboardKeymanPackage {
  //        try ResourceFileManager.shared.install(resourceWithID: TestUtils.Keyboards.fv_sencoten.fullID, from: sencoten_kbd_kmp)
  //      } else {
  //          XCTFail()
  //      }
  //
  //      if let mtntKMP = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.mtntKMP) as? LexicalModelKeymanPackage {
  //        try ResourceFileManager.shared.install(resourceWithID: TestUtils.LexicalModels.mtnt.fullID,
  //          from: mtntKMP)
  //      } else {
  //          XCTFail()
  //      }
  //
  //      if let sencoten_lm_kmp = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.sencotenKMP) as? LexicalModelKeymanPackage {
  //        try ResourceFileManager.shared.install(resourceWithID: TestUtils.LexicalModels.sencoten.fullID,
  //          from: sencoten_lm_kmp)
  //      } else {
  //          XCTFail()
  //      }
  //
  //    } catch {
  //      XCTFail("File system commands failed.")
  //    }
  //
  //    // Set the desired version entry for your format, if different from the actual local version.
  //    // Only use if you're sure the specified version has a matching file format.
  //    Storage.active.userDefaults.lastEngineVersion = Version.packageBasedFileReorg
  //    Storage.active.userDefaults.migrationLevel = 20 // migratedForKMP
  //
  //    // Note:  you may need to manually pause at a certain (marked) point within this method
  //    // for everything to successfully write out!
  //    if let saveState = try? TestUtils.EngineStateBundler.createBundle(withName: "Early 14.0 with defaults") {
  //      self.add(saveState)
  //    } else {
  //      XCTFail()
  //    }
  //  }
  
  func testSimpleEarly14Migration() {
    // A case where the user has both SENCOTEN installed for both keyboard & lexical model.
    // Mirrors `testNoDefaultsEarly14Migration`, but where sil_euro_latin
    // was not removed by the user; in contrast, the base resources
    // should be auto-updated.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.simple_14)
    Migrations.migrate(storage: Storage.active)
    Migrations.updateResources(storage: Storage.active)
    
    let userDefaults = Storage.active.userDefaults
    let userKeyboards = userDefaults.userKeyboards ?? []
    let userLexicalModels = userDefaults.userLexicalModels ?? []
    
    XCTAssertEqual(userKeyboards.count, 2)
    // Because there's a lexical model update, it installs the whole package.
    // Not exactly ideal, but correct for pre-existing behavior, which
    // installs the whole default package instead of a single language-code pairing.
    XCTAssertEqual(userLexicalModels.count, 4)
    
    //[s]il_[e]uro_[l]atin
    let kbdSEL = userKeyboards.first(where: { $0.fullID == TestUtils.Keyboards.sil_euro_latin.fullID })
    XCTAssertNotNil(kbdSEL)
    // Because there's a keyboard update (1.9.3 vs 1.9.1, at the time of writing).
    // we expect a more recent version than the testing version.
    XCTAssertGreaterThan(Version(kbdSEL!.version)!, Version(TestUtils.Keyboards.sil_euro_latin.version)!)
    XCTAssertTrue(userLexicalModels.contains(where: { $0.fullID == TestUtils.LexicalModels.mtnt.fullID }))
    
    XCTAssertTrue(userKeyboards.contains(where: { $0.fullID == TestUtils.Keyboards.fv_sencoten.fullID }))
    XCTAssertTrue(userLexicalModels.contains(where: { $0.fullID == TestUtils.LexicalModels.sencoten.fullID }))
  }
  
  func testNoDefaultsEarly14Migration() {
    // A case where the user only has SENCOTEN installed for both keyboard & lexical model.
    // sil_euro_latin was explicitly removed by the user.
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.noDefault_14)
    Migrations.migrate(storage: Storage.active)
    Migrations.updateResources(storage: Storage.active)
    
    let userDefaults = Storage.active.userDefaults
    let userKeyboards = userDefaults.userKeyboards ?? []
    let userLexicalModels = userDefaults.userLexicalModels ?? []
    
    XCTAssertEqual(userKeyboards.count, 1)
    XCTAssertEqual(userLexicalModels.count, 1)
    
    XCTAssertFalse(userKeyboards.contains(where: { $0.fullID == TestUtils.Keyboards.sil_euro_latin.fullID }))
    XCTAssertFalse(userLexicalModels.contains(where: { $0.fullID == TestUtils.LexicalModels.mtnt.fullID }))
    
    XCTAssertTrue(userKeyboards.contains(where: { $0.fullID == TestUtils.Keyboards.fv_sencoten.fullID }))
    XCTAssertTrue(userLexicalModels.contains(where: { $0.fullID == TestUtils.LexicalModels.sencoten.fullID }))
  }
  
  func testComplexVersion13Migration() {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.cloud_to_kmp_13)
    Migrations.migrate(storage: Storage.active)
    Migrations.updateResources(storage: Storage.active)
    
    let userDefaults = Storage.active.userDefaults
    let userKeyboards = userDefaults.userKeyboards
    
    // The fun thing to test here - sil_euro_latin is first wrapped with an auto-generated
    // kmp.json, THEN updated with the actual KMP.
    
    let sil_euro_latin_kbds = userKeyboards!.filter { return $0.id == "sil_euro_latin" }
    
    sil_euro_latin_kbds.forEach {
      XCTAssertEqual($0.packageID, "sil_euro_latin")
      XCTAssertEqual($0.version, Defaults.keyboard.version)
    }
    
    let sil_euro_latin_package = ResourceFileManager.shared.installedPackages.first(where: { $0.id == "sil_euro_latin" }) as! KeyboardKeymanPackage
    
    XCTAssertFalse(sil_euro_latin_package.metadata.isAutogeneratedWrapper)
    
    // As it's installing a true package, there will be files existing here that won't exist
    // for the others.
    let validationFilenames = ["kmp.inf", "readme.htm", "welcome.htm", "sil_euro_latin.kmx", "DejaVuSans.ttf", "currency.png", "usage.htm"]
    validationFilenames.forEach {
      let file = sil_euro_latin_package.sourceFolder.appendingPathComponent($0)
      XCTAssertTrue(FileManager.default.fileExists(atPath: file.path), "Expected file \(file.path) missing")
    }
  }
  
  func testVersion13ResourceMigration() {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.simple_13)
    Migrations.updateResources(storage: Storage.active)
    
    let userDefaults = Storage.active.userDefaults
    
    // SIL EuroLatin should be updated to the currently-bundled version.  The lexical model version should be unchanged.
    
    let defaultKbd = userDefaults.userKeyboards![0]
    XCTAssertEqual(defaultKbd.id, Defaults.keyboard.id)
    XCTAssertEqual(defaultKbd.version, Defaults.keyboard.version)
    let keyboardURL = Storage.active.resourceURL(for: Defaults.keyboard)!
    XCTAssert(FileManager.default.fileExists(atPath: keyboardURL.path))
  }
  
  func testVersion13CloudToKMPMigration() throws {
    TestUtils.Migrations.applyBundleToFileSystem(TestUtils.Migrations.cloud_to_kmp_13)
    
    let initialUserKeyboards = Storage.active.userDefaults.userKeyboards!
    let initialUserModels = Storage.active.userDefaults.userLexicalModels!
    
    // A baseline load-check; confirms we have the correct resource count at the start.
    XCTAssertEqual(initialUserKeyboards.count, 7)
    XCTAssertEqual(initialUserModels.count, 6)
    
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
    
    // Pass 2:  do we get the expected package metadata and file structure?
    let installedPackages = ResourceFileManager.shared.installedPackages
    installedPackages.forEach { package in
      // A bit lazy, but at least we'll know which package(s) the error occurs for.
      let generalMessage = "Unexpected value for package \(package.id)"
      
      XCTAssertEqual(package.resources.count, 1, generalMessage)
      let metadataFile = package.sourceFolder.appendingPathComponent("kmp.json")
      XCTAssertTrue(FileManager.default.fileExists(atPath: metadataFile.path), generalMessage)
      
      // "sil_sahu" has a version-matched locally-cached KMP, so it should be migrated based on
      // that local KMP.
      if package.id == "sil_sahu" {
        XCTAssertFalse(package.metadata.isAutogeneratedWrapper, generalMessage)
        XCTAssertNotEqual(package.version, Version(KMPInfo.AUTOGEN_VERSION.description), generalMessage)
        XCTAssertEqual(package.version, Version("1.0")) // The version of the stored sil_sahu package.
        
        // As it's installing a true package, there will be files existing here that won't exist
        // for the others.
        let validationFilenames = ["kmp.inf", "readme.htm", "welcome.htm", "sil_sahu.kmx"]
        validationFilenames.forEach {
          let file = package.sourceFolder.appendingPathComponent($0)
          XCTAssertTrue(FileManager.default.fileExists(atPath: file.path), "Expected file \(file.path) missing for package \(package.id)")
        }
        // The others get autogenerated version wrappers.
        // Even sil_cameroon_qwerty, which has a locally-cached KMP... but the version's out of date.
      } else {
        XCTAssertTrue(package.metadata.isAutogeneratedWrapper, generalMessage)
        XCTAssertEqual(package.version, Version(KMPInfo.AUTOGEN_VERSION.description), generalMessage)
      }
      
      // .installableResourceSets[0] -> the set of LanguageResources represented by the
      // one expected script-file-resource in each 'package'.
      switch package.id {
      case "sil_euro_latin":
        XCTAssertEqual(package.installableResourceSets[0].count, 2, generalMessage)
      case "nrc.en.mtnt":
        XCTAssertEqual(package.installableResourceSets[0].count, 3, generalMessage)
      case "nrc.str.sencoten":
        XCTAssertEqual(package.installableResourceSets[0].count, 2, generalMessage)
      default:
        XCTAssertEqual(package.installableResourceSets[0].count, 1, generalMessage)
      }
    }
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
