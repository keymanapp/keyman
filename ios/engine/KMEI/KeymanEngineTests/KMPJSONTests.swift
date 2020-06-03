//
//  PackageJSONTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class KMPJSONTests: XCTestCase {
  private func loadObjectFromJSON<Type: Decodable>(at file: URL) throws -> Type {
    let fileContents: String = try String(contentsOf: file, encoding: .utf8).replacingOccurrences(of: "\r", with: "")
    let jsonData = fileContents.data(using: .utf8)!
    let decoder = JSONDecoder()
    return try decoder.decode(Type.self, from: jsonData)
  }

  func testLanguageDecoding() throws {
    let lang_en: KMPLanguage = try loadObjectFromJSON(at: TestUtils.PackageJSON.language_en)

    XCTAssertEqual(lang_en.name, "English")
    XCTAssertEqual(lang_en.languageId, "en")

    let lang_km: KMPLanguage = try loadObjectFromJSON(at: TestUtils.PackageJSON.language_km)

    XCTAssertEqual(lang_km.name, "Central Khmer (Khmer, Cambodia)")
    XCTAssertEqual(lang_km.languageId, "km")
  }

  // A helper method, since these are useful in two separate tests.
  func keyboard_khmer_angkor_assertions(_ khmer_angkor: KMPKeyboard) {
    XCTAssertEqual(khmer_angkor.languages.count, 1)
    XCTAssertEqual(khmer_angkor.languages[0].name, "Central Khmer (Khmer, Cambodia)")
    XCTAssertEqual(khmer_angkor.languages[0].languageId, "km")

    XCTAssertEqual(khmer_angkor.name, "Khmer Angkor")
    XCTAssertEqual(khmer_angkor.keyboardId, "khmer_angkor")
    XCTAssertEqual(khmer_angkor.version, "1.0.6")
    XCTAssertEqual(khmer_angkor.font, "Mondulkiri-R.ttf")
    XCTAssertEqual(khmer_angkor.osk, "Mondulkiri-R.ttf")
    XCTAssertEqual(khmer_angkor.isRTL, false)
  }

  func testKeyboardDecoding() throws {
    let khmer_angkor: KMPKeyboard = try loadObjectFromJSON(at: TestUtils.PackageJSON.keyboard_khmer_angkor)
    keyboard_khmer_angkor_assertions(khmer_angkor)
  }

  // A helper method, since these are useful in two separate tests.
  func lexical_model_nrc_en_mtnt_assertions(_ nrc_en_mtnt: KMPLexicalModel, version: String? = nil) {
    XCTAssertEqual(nrc_en_mtnt.name, "English dictionary (MTNT)")
    XCTAssertEqual(nrc_en_mtnt.lexicalModelId, "nrc.en.mtnt")
    // Our example case does not define either of these two values.
    // One (isRTL), we assume a default value for.
    XCTAssertEqual(nrc_en_mtnt.version, version)
    XCTAssertEqual(nrc_en_mtnt.isRTL, false)

    XCTAssertEqual(nrc_en_mtnt.languages.count, 3)
    XCTAssertEqual(nrc_en_mtnt.languages[0].languageId, "en")
    XCTAssertEqual(nrc_en_mtnt.languages[1].languageId, "en-us")
    XCTAssertEqual(nrc_en_mtnt.languages[2].languageId, "en-ca")
  }

  func testLexicalModelDecoding() throws {
    let nrc_en_mtnt: KMPLexicalModel = try loadObjectFromJSON(at: TestUtils.PackageJSON.model_nrc_en_mtnt)
    lexical_model_nrc_en_mtnt_assertions(nrc_en_mtnt)
  }

  func kmp_info_khmer_angkor_assertions(_ khmer_angkor: KMPMetadata) {
    XCTAssertTrue(khmer_angkor.isValid)
    XCTAssertEqual(khmer_angkor.packageType, KMPMetadata.PackageType.Keyboard)

    XCTAssertNotNil(khmer_angkor.keyboards)
    XCTAssertEqual(khmer_angkor.keyboards!.count, 1)
    keyboard_khmer_angkor_assertions(khmer_angkor.keyboards![0])

    XCTAssertNil(khmer_angkor.lexicalModels)

    XCTAssertNotNil(khmer_angkor.files)
    XCTAssertEqual(khmer_angkor.files!.count, 16)

    XCTAssertNotNil(khmer_angkor.info)
    XCTAssertEqual(khmer_angkor.info!.version!.description, "1.0.6")
    XCTAssertNil(khmer_angkor.info!.version!.url)
    XCTAssertEqual(khmer_angkor.info!.author!.description, "Makara Sok")
    XCTAssertEqual(khmer_angkor.info!.author!.url, "mailto:makara@keyman.com")

    XCTAssertEqual(khmer_angkor.system.fileVersion, "7.0")

    XCTAssertNil(khmer_angkor.options.graphicFile)
    XCTAssertNil(khmer_angkor.options.readmeFile)
  }

  func testKeyboardPackageInfoDecoding() throws {
    let khmer_angkor: KMPMetadata = try loadObjectFromJSON(at: TestUtils.PackageJSON.kmp_json_khmer_angkor)
    kmp_info_khmer_angkor_assertions(khmer_angkor)
  }

  func kmp_info_nrc_en_mtnt_assertions(_ nrc_en_mtnt: KMPMetadata, version: String? = nil) {
    XCTAssertTrue(nrc_en_mtnt.isValid)
    XCTAssertEqual(nrc_en_mtnt.packageType, KMPMetadata.PackageType.LexicalModel)

    XCTAssertNotNil(nrc_en_mtnt.lexicalModels)
    XCTAssertEqual(nrc_en_mtnt.lexicalModels!.count, 1)
    lexical_model_nrc_en_mtnt_assertions(nrc_en_mtnt.lexicalModels![0], version: version)

    XCTAssertNil(nrc_en_mtnt.keyboards)

    XCTAssertNotNil(nrc_en_mtnt.files)
    XCTAssertEqual(nrc_en_mtnt.files!.count, 1)

    XCTAssertNotNil(nrc_en_mtnt.info)
    XCTAssertEqual(nrc_en_mtnt.info!.version!.description, "0.1.4")
    XCTAssertNil(nrc_en_mtnt.info!.version!.url)
    XCTAssertEqual(nrc_en_mtnt.info!.author!.description, "Eddie Antonio Santos")
    XCTAssertEqual(nrc_en_mtnt.info!.author!.url, "mailto:easantos@ualberta.ca")

    XCTAssertEqual(nrc_en_mtnt.system.fileVersion, "12.0")

    XCTAssertNil(nrc_en_mtnt.options.graphicFile)
    XCTAssertNil(nrc_en_mtnt.options.readmeFile)
  }

  func testLexicalModelPackageInfoDecoding() throws {
    let nrc_en_mtnt: KMPMetadata = try loadObjectFromJSON(at: TestUtils.PackageJSON.kmp_json_nrc_en_mtnt)
    kmp_info_nrc_en_mtnt_assertions(nrc_en_mtnt)
  }
}
