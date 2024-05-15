//
//  KeyboardSearchTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/22/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class KeyboardSearchTests: XCTestCase {
  var mockedURLSession: TestUtils.Downloading.URLSessionMock?
  var downloadManager: ResourceDownloadManager!
  
  override func setUp() {
    mockedURLSession = TestUtils.Downloading.URLSessionMock()
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: true)
  }
  
  override func tearDownWithError() throws {
    // Resets resource directories for a clean slate.
    TestUtils.standardTearDown()
    
    let queueWasCleared = mockedURLSession!.queueIsEmpty
    mockedURLSession = nil
    
    if !queueWasCleared {
      throw NSError(domain: "Keyman",
                    code: 4,
                    userInfo: [NSLocalizedDescriptionKey: "A test did not fully utilize its queued mock results!"])
    }
  }
  
  func testFinalizeNoLanguage() {
    let kbdExpectation = XCTestExpectation()
    
    let kbdBlock: KeyboardSearchViewController.SelectionCompletedHandler<FullKeyboardID> = { searchResult in
      switch searchResult {
      case .untagged(let packageKey, let url):
        XCTAssertEqual(packageKey, TestUtils.Packages.Keys.khmer_angkor)
        XCTAssertNotNil(url)
      default:
        XCTFail()
      }
      
      kbdExpectation.fulfill()
    }
    
    let searchNoLang = KeyboardSearchViewController(languageCode: nil,
                                                    withSession: mockedURLSession!,
                                                    keyboardSelectionBlock: kbdBlock)
    searchNoLang.finalize(with: UniversalLinks.ParsedKeyboardInstallLink(keyboard_id: "khmer_angkor", lang_id: nil))
    
    wait(for: [kbdExpectation], timeout: 5)
  }
  
  func testFinalizeWithLanguage() {
    let kbdExpectation = XCTestExpectation()
    let kbdBlock: KeyboardSearchViewController.SelectionCompletedHandler<FullKeyboardID> = { searchResult in
      switch searchResult {
      case .tagged(let packageKey, let url, let resourceKey):
        XCTAssertEqual(packageKey, TestUtils.Packages.Keys.sil_euro_latin)
        XCTAssertNotNil(url)
        XCTAssertEqual(resourceKey, TestUtils.Keyboards.sil_euro_latin.fullID)
      default:
        XCTFail()
      }
      
      kbdExpectation.fulfill()
    }
    
    let search = KeyboardSearchViewController(languageCode: nil,
                                              withSession: mockedURLSession!,
                                              keyboardSelectionBlock: kbdBlock)
    search.finalize(with: UniversalLinks.ParsedKeyboardInstallLink(keyboard_id: "sil_euro_latin", lang_id: "en"))
    
    wait(for: [kbdExpectation], timeout: 5)
  }
  
  
  func testDefaultDownloadClosureTaggedSuccess() throws {
    // Step 1:  mocking.
    let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession!.queueMockResult(.download(packageDownloadTask))
    
    let downloadExpectation = XCTestExpectation(description: "Keyboard package download should complete")
    
    // Step 2 - build closure
    let closure = KeyboardSearchViewController.defaultDownloadClosure(withDownloadManager: downloadManager) { result in
      if case let .success(package, fullID) = result {
        XCTAssertEqual(package.key, TestUtils.Packages.Keys.khmer_angkor)
        XCTAssertEqual(fullID as? FullKeyboardID, TestUtils.Keyboards.khmer_angkor.fullID)
      } else {
        XCTFail("keyboard installation did not succeed.")
      }
      
      downloadExpectation.fulfill()
    }
    
    // Step 3 - run closure
    closure(.tagged(TestUtils.Packages.Keys.khmer_angkor, TestUtils.Keyboards.khmerAngkorKMP, TestUtils.Keyboards.khmer_angkor.fullID))
    
    wait(for: [downloadExpectation], timeout: 5, enforceOrder: true)
  }
  
  func testDefaultDownloadClosureUntaggedSuccess() throws {
    // Step 1:  mocking.
    let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession!.queueMockResult(.download(packageDownloadTask))
    
    let downloadExpectation = XCTestExpectation(description: "Keyboard package download should complete")
    
    // Step 2 - build closure
    let closure = KeyboardSearchViewController.defaultDownloadClosure(withDownloadManager: downloadManager) { result in
      if case let .success(package, fullID) = result {
        XCTAssertEqual(package.key, TestUtils.Packages.Keys.khmer_angkor)
        XCTAssertEqual(fullID as? FullKeyboardID, TestUtils.Keyboards.khmer_angkor.fullID)
      } else {
        XCTFail("keyboard installation did not succeed.")
      }
      
      downloadExpectation.fulfill()
    }
    
    // Step 3 - run closure
    closure(.untagged(TestUtils.Packages.Keys.khmer_angkor, TestUtils.Keyboards.khmerAngkorKMP))
    
    wait(for: [downloadExpectation], timeout: 5, enforceOrder: true)
  }
  
  func testDefaultDownloadClosureTaggedError() throws {
    // Step 1:  mocking.
    let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: TestUtils.mockedError)
    mockedURLSession!.queueMockResult(.download(packageDownloadTask))
    
    let downloadExpectation = XCTestExpectation(description: "Keyboard package download should complete")
    
    // Step 2 - build closure
    let closure = KeyboardSearchViewController.defaultDownloadClosure(withDownloadManager: downloadManager) { result in
      if case .error(_) = result {
        // Success!
      } else {
        XCTFail("keyboard installation did not result in mocked error.")
      }
      
      downloadExpectation.fulfill()
    }
    
    // Step 3 - run closure
    closure(.tagged(TestUtils.Packages.Keys.khmer_angkor, TestUtils.Keyboards.khmerAngkorKMP, TestUtils.Keyboards.khmer_angkor.fullID))
    
    wait(for: [downloadExpectation], timeout: 5, enforceOrder: true)
  }
  
  func testDefaultDownloadClosureCancelled() throws {
    // Step 1:  mocking.
    let downloadExpectation = XCTestExpectation(description: "Keyboard package download should complete")
    
    // Step 2 - build closure
    let closure = KeyboardSearchViewController.defaultDownloadClosure(withDownloadManager: downloadManager) { result in
      if case .cancelled = result {
        // Success!
      } else {
        XCTFail("keyboard-search cancellation handled improperly.")
      }
      
      downloadExpectation.fulfill()
    }
    
    // Step 3 - run closure
    closure(.cancelled)
    
    wait(for: [downloadExpectation], timeout: 5, enforceOrder: true)
  }
}
