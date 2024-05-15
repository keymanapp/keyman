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
    let mockedResultEn = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedResultEn))
    
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
    
    let mockedResultKm = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_km, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedResultKm))
    
    Queries.LexicalModel.fetch(forLanguageCode: "km", withSession: mockedURLSession!) { results, error in
      if let _ = error {
        XCTFail(String(describing: error))
        return
      }
      XCTAssertNotNil(results)
      
      if let results = results {
        XCTAssertEqual(results.count, 0)
      }
    }
  }
  
  func testFetchPackageKey() throws {
    let mockedResultEn = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedResultEn))
    
    Queries.LexicalModel.fetchModels(forLanguageCode: "en", withSession: mockedURLSession!) { results, error in
      if error != nil {
        XCTFail(String(describing: error))
        return
      }
      XCTAssertNotNil(results)
      
      if let results = results {
        XCTAssertTrue(results.contains { tuple in
          let lexicalModel = tuple.0
          return lexicalModel.fullID == TestUtils.LexicalModels.mtnt.fullID &&
          lexicalModel.packageKey == TestUtils.Packages.Keys.nrc_en_mtnt})
      }
    }
    
    let mockedResultKm = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_km, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedResultKm))
    
    Queries.LexicalModel.fetchModels(forLanguageCode: "km", withSession: mockedURLSession!) { results, error in
      if error != nil {
        XCTFail(String(describing: error))
        return
      }
      XCTAssertNotNil(results)
      
      if let results = results {
        XCTAssertEqual(results.count, 0)
      }
    }
  }
}
