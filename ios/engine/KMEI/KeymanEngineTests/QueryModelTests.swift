//
//  QueryModelTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/8/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class QueryModelTests: XCTestCase {
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
   * A rigorous test of our model-search query code that runs against a fixture copied from an actual api.keyman.com query return (8 Jul 2020).
   */
  func testMockedFetchParse() throws {
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedResult))

    // As it's a mocked fetch, it happens synchronously.
    Queries.LexicalModel.fetch(forLanguageCode: "en", withSession: mockedURLSession!) { results, error in
      if let _ = error {
        XCTFail(String(describing: error))
        return
      }
      XCTAssertNotNil(results)

      if let results = results {
        XCTAssertEqual(results.count, 1)

        if results.count > 0 {
          let result = results[0]
          XCTAssertEqual(result.id, "nrc.en.mtnt")
          XCTAssertEqual(result.languages.count, 3)
        }
      }
    }
  }
}
