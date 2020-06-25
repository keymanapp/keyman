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
  let PACKAGE_HOST_DOMAIN = "https://downloads.keyman.com/"
  /**
   * A rigorous test of our package-version query code that runs against the actual, live api.keyman.com endpoint.
   */
  func testFetch() throws {
    let fullIDs = [TestUtils.Keyboards.khmer_angkor.fullID,
                   TestUtils.Keyboards.sil_euro_latin.fullID,
                   TestUtils.LexicalModels.mtnt.fullID]
    let expectation = XCTestExpectation(description: "Package-version query results received")
    Queries.PackageVersion.fetch(for: fullIDs) { results, error in
      XCTAssertNil(error)
      XCTAssertNotNil(results)

      if let results = results {
        XCTAssertNotNil(results.keyboards)
        XCTAssertNotNil(results.models)

        if let keyboardVersions = results.keyboards, let modelVersions = results.models {
          XCTAssertEqual(keyboardVersions.count, 2)
          XCTAssertEqual(modelVersions.count, 1)

          keyboardVersions.forEach { entry in
            if let error = entry.value as? Queries.PackageVersion.ResultError {
              XCTFail(String(describing: error))
            }
          }

          if let khmer_angkor = keyboardVersions["khmer_angkor"] as? Queries.PackageVersion.ResultEntry {
            XCTAssertGreaterThanOrEqual(Version(khmer_angkor.version)!,
                                        Version(TestUtils.Keyboards.khmer_angkor.version)!)
            XCTAssertTrue(khmer_angkor.packageURL.hasPrefix(self.PACKAGE_HOST_DOMAIN))
            XCTAssertTrue(khmer_angkor.packageURL.hasSuffix("khmer_angkor.kmp"))
          }

          if let sil_euro_latin = keyboardVersions["sil_euro_latin"] as? Queries.PackageVersion.ResultEntry {
            XCTAssertGreaterThanOrEqual(Version(sil_euro_latin.version)!,
                                        Version(TestUtils.Keyboards.sil_euro_latin.version)!)
            XCTAssertTrue(sil_euro_latin.packageURL.hasPrefix(self.PACKAGE_HOST_DOMAIN))
            XCTAssertTrue(sil_euro_latin.packageURL.hasSuffix("sil_euro_latin.kmp"))
          }

          modelVersions.forEach { entry in
            if let error = entry.value as? Queries.PackageVersion.ResultError {
              XCTFail(String(describing: error))
            }
          }

          if let nrc_en_mtnt = keyboardVersions["nrc_en_mtnt"] as? Queries.PackageVersion.ResultEntry {
            XCTAssertGreaterThanOrEqual(Version(nrc_en_mtnt.version)!,
                                        Version(TestUtils.LexicalModels.mtnt.version)!)
            XCTAssertTrue(nrc_en_mtnt.packageURL.hasPrefix(self.PACKAGE_HOST_DOMAIN))
            XCTAssertTrue(nrc_en_mtnt.packageURL.hasSuffix("nrc.en.mtnt.model.kmp"))
          }
        }
      }

      expectation.fulfill()
    }

    wait(for: [expectation], timeout: 5)
  }
}
