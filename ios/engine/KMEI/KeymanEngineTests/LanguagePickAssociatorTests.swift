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
  }

  override func tearDownWithError() throws {
    // Resets resource directories for a clean slate.
    TestUtils.standardTearDown()
  }

  func testSequentialQuadPick() throws {
    let expectation = XCTestExpectation()

    let associator = LanguagePickAssociator<InstallableLexicalModel>(searchWith: mockedSearchCallback) { results in
      if results.count == 2 {
        XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
        XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
      } else {
        XCTFail()
      }
      expectation.fulfill()
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

    let associator = LanguagePickAssociator<InstallableLexicalModel>(searchWith: mockedSearchCallback) { results in
      if results.count == 2 {
        XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
        XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
      } else {
        XCTFail()
      }
      expectation.fulfill()
    }

    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.pickerFinalized()

    wait(for: [expectation], timeout: 5)
  }

  func testDeselect() throws {
    let expectation = XCTestExpectation()

    let associator = LanguagePickAssociator<InstallableLexicalModel>(searchWith: mockedSearchCallback) { results in
      if results.count == 1 {
        XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
        XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
      } else {
        XCTFail()
      }
      expectation.fulfill()
    }

    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.deselectLanguages(Set(["str"]))
    associator.pickerFinalized()

    wait(for: [expectation], timeout: 5)
  }

  func testFickleSelection() throws {
    // Ppointer magic for detecting multiple calls to the callback.
    class Flag {
      var value = false
    }

    let flag = Flag()

    let singleSearchCallback: LanguagePickAssociator.AssociationSearcher = { lgCodes, callback in
      self.mockedSearchCallback(lgCodes) { result in
        callback(result)
        XCTAssertFalse(flag.value)
        flag.value = true
      }
    }

    // Uncomment to test validity of test code.
    // singleSearchCallback(Set(["en"])) { _ in }

    let expectation = XCTestExpectation()

    let associator = LanguagePickAssociator<InstallableLexicalModel>(searchWith: singleSearchCallback) { results in
      if results.count == 1 {
        XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
        XCTAssertTrue(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
      } else {
        XCTFail()
      }
      expectation.fulfill()
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

    let associator = LanguagePickAssociator<InstallableLexicalModel>(searchWith: mockedSearchCallback) { results in
      if results.count == 0 {
        XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
        XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
      } else {
        XCTFail()
      }
      expectation.fulfill()
    }

    associator.pickerInitialized()

    // For actual use with the package-install language picker, this should be impossible.
    // Still, best to test it anyway... especially since it may see use outside said picker.
    associator.pickerFinalized()

    wait(for: [expectation], timeout: 5)
  }

  func testSelectionsDismissed() throws {
    let expectation = XCTestExpectation()

    let associator = LanguagePickAssociator<InstallableLexicalModel>(searchWith: mockedSearchCallback) { results in
      if results.count == 0 {
        XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
        XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
      } else {
        XCTFail()
      }
      expectation.fulfill()
    }

    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.pickerDismissed()

    wait(for: [expectation], timeout: 5)
  }

//  func testSelectionsDeinit() throws {
//    let expectation = XCTestExpectation()
//
//    // Uses scoping to test variable de-init behavior.
//    autoreleasepool {
//      var associator: LanguagePickAssociator<InstallableLexicalModel>? = LanguagePickAssociator(searchWith: mockedSearchCallback) { results in
//        if results.count == 0 {
//          XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
//          XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
//        } else {
//          XCTFail()
//        }
//        expectation.fulfill()
//      }
//
//      associator!.pickerInitialized()
//      associator!.selectLanguages(Set(["en", "km", "str", "foobar"]))
//      associator = nil
//    }
//
//    wait(for: [expectation], timeout: 5)
//  }

  func testQueryCompletionDelay() throws {
    let expectation = XCTestExpectation()
    expectation.isInverted = true
    let delayedExpectation = XCTestExpectation()
    let delaySync = DispatchGroup()

    delaySync.enter()
    let delayedSearchCallback: LanguagePickAssociator.AssociationSearcher = { lgCodes, callback in
      self.mockedSearchCallback(lgCodes) { result in
        // It will only return when we _allow_ it to return (via delaySync.leave).
        delaySync.notify(queue: DispatchQueue.main) {
          callback(result)
        }
      }
    }

    let associator = LanguagePickAssociator<InstallableLexicalModel>(searchWith: delayedSearchCallback) { results in
      if results.count == 0 {
        XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_en_mtnt))
        XCTAssertFalse(results.keys.contains(TestUtils.Packages.Keys.nrc_str_sencoten))
      } else {
        XCTFail()
      }
      expectation.fulfill()
      delayedExpectation.fulfill()
    }

    associator.pickerInitialized()
    associator.selectLanguages(Set(["en", "km", "str", "foobar"]))
    associator.pickerDismissed()

    wait(for: [expectation], timeout: 1)

    delaySync.leave()

    wait(for: [delayedExpectation], timeout: 1)
  }
}
