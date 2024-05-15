//
//  LanguagePickAssociatorTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 8/3/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class LanguagePickAssociatorTests: XCTestCase {
  class Counter {
    var count = 0
    
    func increment() {
      count += 1
    }
    
    var value: Int {
      return count
    }
  }
  
  var mockedURLSession: TestUtils.Downloading.URLSessionMock!
  
  let mockedSearchCallback: LanguagePickAssociator.AssociationSearcher = { lgCodes, callback in
    var searchResult: [String: (KeymanPackage.Key, URL)?] = [:]
    
    // We aren't downloading these models, so a placeholder URL is fine.
    let placeholderURL = URL.init(string: "http://place.holder.com")!
    
    lgCodes.forEach { lgCode in
      switch lgCode {
      case "en":
        searchResult["en"] = (TestUtils.Packages.Keys.nrc_en_mtnt, placeholderURL)
      case "str":
        searchResult["str"] = (TestUtils.Packages.Keys.nrc_str_sencoten, placeholderURL)
      default:
        searchResult[lgCode] = nil
      }
    }
    
    callback(searchResult)
  }
  
  override func setUp() {
    mockedURLSession = TestUtils.Downloading.URLSessionMock()
  }
  
  override func tearDownWithError() throws {
    TestUtils.standardTearDown()
    
    let queueWasCleared = mockedURLSession!.queueIsEmpty
    mockedURLSession = nil
    
    if !queueWasCleared {
      throw NSError(domain: "Keyman",
                    code: 4,
                    userInfo: [NSLocalizedDescriptionKey: "A test did not fully utilize its queued mock results!"])
    }
  }
  
  func testSequentialQuadPick() throws {
    let expectation = XCTestExpectation()
    
    let tracker = Counter()
    
    let associator = LanguagePickAssociator(searchWith: mockedSearchCallback) { state in
      switch state {
      case .cancelled:
        XCTFail()
      case .inProgress(let queriesComplete, let modelsFound):
        XCTAssertLessThanOrEqual(queriesComplete, 4)
        XCTAssertLessThanOrEqual(modelsFound, 2)
        XCTAssertLessThanOrEqual(modelsFound, queriesComplete)
        tracker.increment()
      case .complete(let queriesComplete, let results):
        XCTAssertEqual(queriesComplete, 4)
        if results.count == 2 {
          XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
          XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
        } else {
          XCTFail()
        }
        XCTAssertEqual(tracker.value, 4)
        expectation.fulfill()
      }
    }
    
    associator.pickerInitialized()
    associator.selectLanguages(Set(["en"]))
    associator.selectLanguages(Set(["km"]))
    associator.selectLanguages(Set(["str"]))
    associator.selectLanguages(Set(["foobar"]))
    associator.pickerFinalized()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testSimultaneousQuadPick() throws {
    let expectation = XCTestExpectation()
    
    let tracker = Counter()
    
    let associator = LanguagePickAssociator(searchWith: mockedSearchCallback) { state in
      switch state {
      case .cancelled:
        XCTFail()
      case .inProgress(let queriesComplete, let modelsFound):
        XCTAssertLessThanOrEqual(queriesComplete, 4)
        XCTAssertLessThanOrEqual(modelsFound, 2)
        XCTAssertLessThanOrEqual(modelsFound, queriesComplete)
        tracker.increment()
      case .complete(let queriesComplete, let results):
        XCTAssertEqual(queriesComplete, 4)
        if results.count == 2 {
          XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
          XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
        } else {
          XCTFail()
        }
        XCTAssertEqual(tracker.value, 1)
        expectation.fulfill()
      }
    }
    
    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.pickerFinalized()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testDeselect() throws {
    let expectation = XCTestExpectation()
    
    let associator = LanguagePickAssociator(searchWith: mockedSearchCallback) { state in
      switch state {
      case .cancelled:
        XCTFail()
      case .inProgress(let queriesComplete, let modelsFound):
        XCTAssertLessThanOrEqual(queriesComplete, 4)
        XCTAssertLessThanOrEqual(modelsFound, 1)
        XCTAssertLessThanOrEqual(modelsFound, queriesComplete)
      case .complete(let queriesComplete, let results):
        XCTAssertEqual(queriesComplete, 4)  // We still queried for "str".
        if results.count == 1 {
          XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
          XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
        } else {
          XCTFail()
        }
        expectation.fulfill()
      }
    }
    
    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.deselectLanguages(Set(["str"]))
    associator.pickerFinalized()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testFickleSelection() throws {
    let expectation = XCTestExpectation()
    
    let tracker = Counter()
    
    let associator = LanguagePickAssociator(searchWith: mockedSearchCallback) { state in
      switch state {
      case .cancelled:
        XCTFail()
      case .inProgress(let queriesComplete, let modelsFound):
        XCTAssertLessThanOrEqual(queriesComplete, 4)
        XCTAssertLessThanOrEqual(modelsFound, 1)
        XCTAssertLessThanOrEqual(modelsFound, queriesComplete)
        tracker.increment()
      case .complete(let queriesComplete, let results):
        XCTAssertEqual(queriesComplete, 4)
        if results.count == 1 {
          XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
          XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
        } else {
          XCTFail()
        }
        XCTAssertEqual(tracker.value, 1)
        expectation.fulfill()
      }
    }
    
    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.deselectLanguages(Set(["str"]))
    associator.selectLanguages(Set(["str"]))
    associator.deselectLanguages(Set(["str"]))
    associator.selectLanguages(Set(["str"]))
    associator.deselectLanguages(Set(["en"]))
    associator.selectLanguages(Set(["en"]))
    associator.deselectLanguages(Set(["en"]))
    associator.pickerFinalized()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testNoSelect() throws {
    let expectation = XCTestExpectation()
    
    let associator = LanguagePickAssociator(searchWith: mockedSearchCallback) { state in
      switch state {
      case .cancelled:
        XCTFail()
      case .inProgress(_, _):
        // If no queries occur, no .inProgress calls should occur.
        XCTFail()
      case .complete(let queriesComplete, let results):
        XCTAssertEqual(queriesComplete, 0)
        XCTAssertEqual(results.count, 0)
        expectation.fulfill()
      }
    }
    
    associator.pickerInitialized()
    
    // For actual use with the package-install language picker, this should be impossible.
    // Still, best to test it anyway... especially since it may see use outside said picker.
    associator.pickerFinalized()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testSelectionsDismissed() throws {
    let expectation = XCTestExpectation()
    
    let tracker = Counter()
    
    let associator = LanguagePickAssociator(searchWith: mockedSearchCallback) { state in
      switch state {
      case .cancelled:
        expectation.fulfill()
      case .inProgress(let queriesComplete, let modelsFound):
        XCTAssertLessThanOrEqual(queriesComplete, 4)
        XCTAssertLessThanOrEqual(modelsFound, 2)
        XCTAssertLessThanOrEqual(modelsFound, queriesComplete)
        tracker.increment()
      case .complete(_, _):
        XCTFail()
      }
    }
    
    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.pickerDismissed()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testSelectionsDeinit() throws {
    let expectation = XCTestExpectation()
    
    let tracker = Counter()
    
    // Uses scoping to test variable de-init behavior.
    do {
      var associator: LanguagePickAssociator? = LanguagePickAssociator(searchWith: mockedSearchCallback) {
        state in
        switch state {
        case .cancelled:
          expectation.fulfill()
        case .inProgress(let queriesComplete, let modelsFound):
          XCTAssertLessThanOrEqual(queriesComplete, 4)
          XCTAssertLessThanOrEqual(modelsFound, 2)
          XCTAssertLessThanOrEqual(modelsFound, queriesComplete)
          tracker.increment()
        case .complete(_, _):
          XCTFail()
        }
      }
      
      associator!.pickerInitialized()
      associator!.selectLanguages(Set(["en", "km", "str", "foobar"]))
      associator = nil
    }
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testQueryCompletionDelay() throws {
    let progressExpectation = XCTestExpectation()
    progressExpectation.isInverted = true
    let immediateCompleteExpectation = XCTestExpectation()
    immediateCompleteExpectation.isInverted = true
    let completeExpectation = XCTestExpectation()
    let delaySync = DispatchGroup()
    
    delaySync.enter()
    let delayedSearchCallback: LanguagePickAssociator.AssociationSearcher = { lgCodes, callback in
      self.mockedSearchCallback(lgCodes) { result in
        // It will only return when we _allow_ it to return (via delaySync.leave).
        // This also delays the progressExpectation.
        delaySync.notify(queue: DispatchQueue.main) {
          callback(result)
        }
      }
    }
    
    let associator = LanguagePickAssociator(searchWith: delayedSearchCallback) { state in
      switch state {
      case .cancelled:
        XCTFail()
      case .inProgress(_, _):
        // We've delayed this as well.
        progressExpectation.fulfill()
      case .complete(_, _):
        immediateCompleteExpectation.fulfill()
        completeExpectation.fulfill()
      }
    }
    
    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.pickerFinalized()
    
    wait(for: [progressExpectation, immediateCompleteExpectation], timeout: 1)
    
    delaySync.leave()
    
    wait(for: [completeExpectation], timeout: 1)
  }
  
  func testLexicalModelSearcher() throws {
    let mocked_en_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    let mocked_km_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_km, error: nil)
    let mocked_str_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_str, error: nil)
    let mocked_none_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_none, error: nil)
    
    // Ensure the mocking order matches the set's iteration order.
    // The calls will be synchronous; it's the callback that isn't.
    let languageSet = Set(["en", "km", "str", "foobar"])
    languageSet.forEach { lgCode in
      switch lgCode {
      case "en":
        mockedURLSession.queueMockResult(.data(mocked_en_query))
      case "km":
        mockedURLSession.queueMockResult(.data(mocked_km_query))
      case "str":
        mockedURLSession.queueMockResult(.data(mocked_str_query))
      default:
        mockedURLSession.queueMockResult(.data(mocked_none_query))
      }
    }
    
    let expectation = XCTestExpectation()
    
    let searchCallback = LanguagePickAssociator.constructLexicalModelSearcher(session: mockedURLSession)
    
    searchCallback(languageSet) { results in
      if results.count == 2 {
        XCTAssertTrue(results.keys.contains("en"))
        XCTAssertTrue(results.keys.contains("str"))
      } else {
        XCTFail()
      }
      expectation.fulfill()
    }
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testLexicalModelSearcherWithFiltering() throws {
    // For this test, we want a pre-installed lexical model for the "en" language code.
    guard let package = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.mtntKMP) as? LexicalModelKeymanPackage else {
      XCTFail()
      return
    }
    
    try ResourceFileManager.shared.install(resourceWithID: TestUtils.LexicalModels.mtnt.fullID, from: package)
    
    let mocked_km_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_km, error: nil)
    let mocked_str_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_str, error: nil)
    let mocked_none_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_none, error: nil)
    
    // Ensure the mocking order matches the set's iteration order.
    // The calls will be synchronous; it's the callback that isn't.
    let languageSet = Set(["en", "km", "str", "foobar"])
    languageSet.forEach { lgCode in
      switch lgCode {
      case "en":
        return // Do not queue - the query should never occur!
      case "km":
        mockedURLSession.queueMockResult(.data(mocked_km_query))
      case "str":
        mockedURLSession.queueMockResult(.data(mocked_str_query))
      default:
        mockedURLSession.queueMockResult(.data(mocked_none_query))
      }
    }
    
    let expectation = XCTestExpectation()
    
    let searchCallback = LanguagePickAssociator.constructLexicalModelSearcher(session: mockedURLSession)
    
    searchCallback(languageSet) { results in
      if results.count == 1 {
        XCTAssertTrue(results.keys.contains("str"))
      } else {
        XCTFail()
      }
      expectation.fulfill()
    }
    
    wait(for: [expectation], timeout: 5)
  }
  
  // A modded version of `testSequentialQuadPick`.  Also includes fine-grained progress checks!
  func testWithQueryIntegration() throws {
    let mocked_en_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    let mocked_km_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_km, error: nil)
    let mocked_str_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_str, error: nil)
    let mocked_none_query = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_none, error: nil)
    
    // Here, we control the order in which the queries arise.
    mockedURLSession.queueMockResult(.data(mocked_en_query))
    mockedURLSession.queueMockResult(.data(mocked_km_query))
    mockedURLSession.queueMockResult(.data(mocked_str_query))
    mockedURLSession.queueMockResult(.data(mocked_none_query))
    
    let expectation = XCTestExpectation()
    let queryExpectations: [String: XCTestExpectation] = [
      "en": XCTestExpectation(),
      "km": XCTestExpectation(),
      "str": XCTestExpectation(),
      "foobar": XCTestExpectation()
    ]
    
    let progressExpectations: [Int: XCTestExpectation] = [
      1: XCTestExpectation(),
      2: XCTestExpectation(),
      3: XCTestExpectation(),
      4: XCTestExpectation()
    ]
    
    let tracker = Counter()
    
    let baseSearchCallback = LanguagePickAssociator.constructLexicalModelSearcher(session: mockedURLSession)
    let searchCallback: LanguagePickAssociator.AssociationSearcher = { lgCodes, completion in
      baseSearchCallback(lgCodes) { results in
        completion(results)
        
        // Allows us to enforce query ordering.
        queryExpectations[lgCodes.first!]!.fulfill()
      }
    }
    
    let associator = LanguagePickAssociator(searchWith: searchCallback) { state in
      switch state {
      case .cancelled:
        XCTFail()
      case .inProgress(1, 1):  // "en" - has lexical model
        tracker.increment()
        progressExpectations[1]!.fulfill()
      case .inProgress(2, 1):  // "km"
        tracker.increment()
        progressExpectations[2]!.fulfill()
      case .inProgress(3, 2):  // "str" - has lexical model
        tracker.increment()
        progressExpectations[3]!.fulfill()
      case .inProgress(4, 2):  // "foobar"
        tracker.increment()
        progressExpectations[4]!.fulfill()
      case .complete(let queriesComplete, let results):
        XCTAssertEqual(queriesComplete, 4)
        if results.count == 2 {
          XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
          XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
        } else {
          XCTFail()
        }
        XCTAssertEqual(tracker.value, 4)
        expectation.fulfill()
      default:
        XCTFail()
      }
    }
    
    associator.pickerInitialized()
    associator.selectLanguages(Set(["en"]))
    wait(for: [queryExpectations["en"]!,     progressExpectations[1]!], timeout: 1)
    associator.selectLanguages(Set(["km"]))
    wait(for: [queryExpectations["km"]!,     progressExpectations[2]!], timeout: 1)
    associator.selectLanguages(Set(["str"]))
    wait(for: [queryExpectations["str"]!,    progressExpectations[3]!], timeout: 1)
    associator.selectLanguages(Set(["foobar"]))
    wait(for: [queryExpectations["foobar"]!, progressExpectations[4]!], timeout: 1)
    associator.pickerFinalized()
    
    wait(for: [expectation], timeout: 5)
  }
}
