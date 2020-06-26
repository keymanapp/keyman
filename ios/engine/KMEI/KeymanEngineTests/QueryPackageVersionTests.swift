//
//  QueryPackageVersionTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/25/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class QueryPackageVersionTests: XCTestCase {
  var mockedURLSession: TestUtils.Downloading.URLSessionMock?

  override func setUp() {
    mockedURLSession = TestUtils.Downloading.URLSessionMock()
  }

  override func tearDownWithError() throws {
    let queueWasCleared = mockedURLSession!.queueIsEmpty
    mockedURLSession = nil

    if !queueWasCleared {
      throw NSError(domain: "Keyman",
                    code: 4,
                    userInfo: [NSLocalizedDescriptionKey: "A test did not fully utilize its queued mock results!"])
    }
  }

  /**
   * A rigorous test of our package-version query code that runs against a fixture copied from an actual api.keyman.com query return (26 Jun 2020).
   */
  func testMockedFetchParse() throws {
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_case_1, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedResult))

    let badKbdFullID = FullKeyboardID(keyboardID: "foo", languageID: "en")
    let badLexFullID = FullLexicalModelID(lexicalModelID: "bar", languageID: "km")
    let fullIDs = [TestUtils.Keyboards.khmer_angkor.fullID,
                   TestUtils.Keyboards.sil_euro_latin.fullID,
                   badKbdFullID,
                   TestUtils.LexicalModels.mtnt.fullID,
                   badLexFullID]

    // As it's a mocked fetch, it happens synchronously.
    Queries.PackageVersion.fetch(for: fullIDs, withSession: mockedURLSession!) { results, error in
      if let _ = error {
        XCTFail(String(describing: error))
        return
      }
      XCTAssertNotNil(results)

      if let results = results {
        XCTAssertNotNil(results.keyboards)
        XCTAssertNotNil(results.models)

        if let keyboardVersions = results.keyboards, let modelVersions = results.models {
          XCTAssertEqual(keyboardVersions.count, 3)
          XCTAssertEqual(modelVersions.count, 2)

          if let khmer_angkor = keyboardVersions["khmer_angkor"] as? Queries.PackageVersion.ResultEntry {
            XCTAssertGreaterThanOrEqual(Version(khmer_angkor.version)!,
                                        Version(TestUtils.Keyboards.khmer_angkor.version)!)
            // Ensures that we are properly decoding the fixture's string entry for this property.
            XCTAssertTrue(khmer_angkor.packageURL.hasSuffix("khmer_angkor.kmp"))
          } else {
            XCTFail()
          }

          if let sil_euro_latin = keyboardVersions["sil_euro_latin"] as? Queries.PackageVersion.ResultEntry {
            XCTAssertGreaterThanOrEqual(Version(sil_euro_latin.version)!,
                                        Version(TestUtils.Keyboards.sil_euro_latin.version)!)
            XCTAssertTrue(sil_euro_latin.packageURL.hasSuffix("sil_euro_latin.kmp"))
          } else {
            XCTFail()
          }

          if let foo = keyboardVersions["foo"] as? Queries.PackageVersion.ResultError {
            XCTAssertEqual(foo.error, "not found")
          } else {
            XCTFail()
          }

          if let nrc_en_mtnt = modelVersions["nrc.en.mtnt"] as? Queries.PackageVersion.ResultEntry {
            XCTAssertGreaterThanOrEqual(Version(nrc_en_mtnt.version)!,
                                        Version(TestUtils.LexicalModels.mtnt.version)!)
            XCTAssertTrue(nrc_en_mtnt.packageURL.hasSuffix("nrc.en.mtnt.model.kmp"))
          } else {
            XCTFail()
          }

          if let bar = modelVersions["bar"] as? Queries.PackageVersion.ResultError {
            XCTAssertEqual(bar.error, "not found")
          }
        }
      }
    }
  }
}
