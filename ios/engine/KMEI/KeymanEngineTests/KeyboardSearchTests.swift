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

  func testTryLinkParse() {
    var tuple = KeyboardSearchViewController.tryParseLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/randomURL")!)

    XCTAssertNil(tuple)

    tuple = KeyboardSearchViewController.tryParseLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/keyboards/install/khmer_angkor")!)

    if let tuple = tuple {
      XCTAssertEqual(tuple.0, "khmer_angkor")
      XCTAssertNil(tuple.1)
    }

    tuple = KeyboardSearchViewController.tryParseLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/keyboards/install/khmer_angkor?bcp47=km")!)

    if let tuple = tuple {
      XCTAssertEqual(tuple.0, "khmer_angkor")
      XCTAssertEqual(tuple.1, "km")
    }

    tuple = KeyboardSearchViewController.tryParseLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/keyboards/install/sil_euro_latin")!)

    if let tuple = tuple {
      XCTAssertEqual(tuple.0, "sil_euro_latin")
      XCTAssertNil(tuple.1)
    }

    tuple = KeyboardSearchViewController.tryParseLink(URL.init(string: "\(KeymanHosts.KEYMAN_COM)/keyboards/install/foo?bcp47=bar")!)

    if let tuple = tuple {
      XCTAssertEqual(tuple.0, "foo")
      XCTAssertEqual(tuple.1, "bar")
    }
  }

  func testFinalizeNoLanguage() {
    let kbdExpectation = XCTestExpectation()
    let lmExpectation = XCTestExpectation()
    let groupExpectation = XCTestExpectation()

    let dispatchGroup = DispatchGroup()

    dispatchGroup.enter()
    let noLangKbdBlock: KeyboardSearchViewController.SelectionCompletedHandler<FullKeyboardID> = { searchResult in
      switch searchResult {
        case .untagged(let packageKey, let url, let langTaggingClosure):
          XCTAssertEqual(packageKey, TestUtils.Packages.Keys.khmer_angkor)
          XCTAssertNotNil(url)

          langTaggingClosure(.none)
        default:
          XCTFail()
      }

      kbdExpectation.fulfill()
      dispatchGroup.leave()
    }

    dispatchGroup.enter()
    let noLangLMBlock: KeyboardSearchViewController.SelectionCompletedHandler<FullLexicalModelID> = { searchResult in
      switch searchResult {
        case .cancelled:
          break
        default:
          XCTFail()
      }

      lmExpectation.fulfill()
      dispatchGroup.leave()
    }

    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    let searchNoLang = KeyboardSearchViewController(languageCode: nil,
                                                    withSession: mockedURLSession!,
                                                    keyboardSelectionBlock: noLangKbdBlock,
                                                    lexicalModelSelectionBlock: noLangLMBlock)
    searchNoLang.finalize(with: "khmer_angkor", for: nil)

    wait(for: [kbdExpectation, lmExpectation, groupExpectation], timeout: 5)
  }

  func testFinalizeWithLanguage() {
    let mockedModelQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedModelQuery))

    let kbdExpectation = XCTestExpectation()
    let lmExpectation = XCTestExpectation()
    let groupExpectation = XCTestExpectation()

    let dispatchGroup = DispatchGroup()

    dispatchGroup.enter()
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
      dispatchGroup.leave()
    }

    dispatchGroup.enter()
    let lmBlock: KeyboardSearchViewController.SelectionCompletedHandler<FullLexicalModelID> = { searchResult in
      switch searchResult {
        case .tagged(let packageKey, let url, let resourceKey):
          XCTAssertEqual(packageKey, TestUtils.Packages.Keys.nrc_en_mtnt)
          XCTAssertNotNil(url)
          XCTAssertEqual(resourceKey, TestUtils.LexicalModels.mtnt.fullID)
        default:
          XCTFail()
      }

      lmExpectation.fulfill()
      dispatchGroup.leave()
    }

    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    let searchNoLang = KeyboardSearchViewController(languageCode: nil,
                                                    withSession: mockedURLSession!,
                                                    keyboardSelectionBlock: kbdBlock,
                                                    lexicalModelSelectionBlock: lmBlock)
    searchNoLang.finalize(with: "sil_euro_latin", for: "en")

    wait(for: [kbdExpectation, lmExpectation, groupExpectation], timeout: 5)
  }

  func testFinalizeWithLanguageModelPreinstalled() throws {
    guard let mtntPackage = try ResourceFileManager.shared.prepareKMPInstall(from: TestUtils.LexicalModels.mtntKMP) as? LexicalModelKeymanPackage else {
      XCTFail()
      return
    }
    try ResourceFileManager.shared.install(resourceWithID: TestUtils.LexicalModels.mtnt.fullID, from: mtntPackage)

    let kbdExpectation = XCTestExpectation()
    let lmExpectation = XCTestExpectation()
    let groupExpectation = XCTestExpectation()

    let dispatchGroup = DispatchGroup()

    dispatchGroup.enter()
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
      dispatchGroup.leave()
    }

    dispatchGroup.enter()
    let lmBlock: KeyboardSearchViewController.SelectionCompletedHandler<FullLexicalModelID> = { searchResult in
      switch searchResult {
        case .cancelled:
          break
        default:
          XCTFail()
      }

      lmExpectation.fulfill()
      dispatchGroup.leave()
    }

    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    let searchNoLang = KeyboardSearchViewController(languageCode: nil,
                                                    withSession: mockedURLSession!,
                                                    keyboardSelectionBlock: kbdBlock,
                                                    lexicalModelSelectionBlock: lmBlock)
    searchNoLang.finalize(with: "sil_euro_latin", for: "en")

    wait(for: [kbdExpectation, lmExpectation, groupExpectation], timeout: 5)
  }

  func testDefaultKeyboardInstallationClosureTaggedSuccess() throws {
    // Step 1:  mocking.
    let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession!.queueMockResult(.download(packageDownloadTask))

    let installExpectation = XCTestExpectation(description: "Keyboard package download & installation should complete")
    let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

    // Step 2 - build closure & synchronization check
    let dispatchGroup = DispatchGroup()

    let closure = KeyboardSearchViewController.defaultKeyboardInstallationClosure(withDownloadManager: downloadManager, dispatchGroup: dispatchGroup) { result in
      if case let .success(fullID) = result {
        XCTAssertEqual(fullID as? FullKeyboardID, TestUtils.Keyboards.khmer_angkor.fullID)
      } else {
        XCTFail("keyboard installation did not succeed.")
      }

      installExpectation.fulfill()
    }

    // As the closure has now been built (and thus, DispatchGroup.enter() called),
    // notification only occurs the callback completes.
    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    // Step 3 - run closure
    closure(.tagged(TestUtils.Packages.Keys.khmer_angkor, TestUtils.Keyboards.khmerAngkorKMP, TestUtils.Keyboards.khmer_angkor.fullID))

    wait(for: [installExpectation, groupExpectation], timeout: 5, enforceOrder: true)

    // Step 4 - verify installation
    XCTAssertTrue(Storage.active.userDefaults.userKeyboards!.contains { $0.fullID == TestUtils.Keyboards.khmer_angkor.fullID })
    XCTAssertNotNil(ResourceFileManager.shared.getInstalledPackage(withKey: TestUtils.Packages.Keys.khmer_angkor))
  }

  func testDefaultKeyboardInstallationClosureTaggedError() throws {
    // Step 1:  mocking.
    let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: TestUtils.mockedError)
    mockedURLSession!.queueMockResult(.download(packageDownloadTask))

    let installExpectation = XCTestExpectation(description: "Keyboard package download & installation should complete")
    let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

    // Step 2 - build closure & synchronization check
    let dispatchGroup = DispatchGroup()

    let closure = KeyboardSearchViewController.defaultKeyboardInstallationClosure(withDownloadManager: downloadManager, dispatchGroup: dispatchGroup) { result in
      if case .error(_) = result {
        // Success!
      } else {
        XCTFail("keyboard installation did not result in mocked error.")
      }

      installExpectation.fulfill()
    }

    // As the closure has now been built (and thus, DispatchGroup.enter() called),
    // notification only occurs the callback completes.
    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    // Step 3 - run closure
    closure(.tagged(TestUtils.Packages.Keys.khmer_angkor, TestUtils.Keyboards.khmerAngkorKMP, TestUtils.Keyboards.khmer_angkor.fullID))

    wait(for: [installExpectation, groupExpectation], timeout: 5, enforceOrder: true)

    // Step 4 - verify lack of installation
    XCTAssertFalse(Storage.active.userDefaults.userKeyboards?.contains { $0.fullID == TestUtils.Keyboards.khmer_angkor.fullID } ?? false)
    XCTAssertNil(ResourceFileManager.shared.getInstalledPackage(withKey: TestUtils.Packages.Keys.khmer_angkor))
  }

 func testDefaultKeyboardInstallationClosureTaggedCancel() throws {
    // Step 1:  mocking.
    let installExpectation = XCTestExpectation(description: "Keyboard package download & installation should complete")
    let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

    // Step 2 - build closure & synchronization check
    let dispatchGroup = DispatchGroup()

    let closure = KeyboardSearchViewController.defaultKeyboardInstallationClosure(withDownloadManager: downloadManager, dispatchGroup: dispatchGroup) { result in
      if case .cancelled = result {
        // Success!
      } else {
        XCTFail("keyboard-search cancellation handled improperly.")
      }

      installExpectation.fulfill()
    }

    // As the closure has now been built (and thus, DispatchGroup.enter() called),
    // notification only occurs the callback completes.
    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    // Step 3 - run closure
    closure(.cancelled)

    wait(for: [installExpectation, groupExpectation], timeout: 5, enforceOrder: true)
  }

  func testDefaultLexicalModelInstallationClosureTaggedSuccess() throws {
    // Step 1 - mocking
    let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)
    mockedURLSession!.queueMockResult(.download(packageDownloadTask))

    let installExpectation = XCTestExpectation(description: "Lexical model package download & installation should complete")
    let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

    // Step 2 - build closure
    let dispatchGroup = DispatchGroup()

    let closure = KeyboardSearchViewController.defaultLexicalModelInstallationClosure(withDownloadManager: downloadManager, dispatchGroup: dispatchGroup) { result in
      if case let .success(fullID) = result {
        XCTAssertEqual(fullID as? FullLexicalModelID, TestUtils.LexicalModels.mtnt.fullID)
      } else {
        XCTFail("Lexical model installation did not succeed.")
      }

      installExpectation.fulfill()
    }

    // As the closure has now been built (and thus, DispatchGroup.enter() called),
    // notification only occurs the callback completes.
    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    // Step 3 - run closure
    closure(.tagged(TestUtils.Packages.Keys.nrc_en_mtnt, TestUtils.LexicalModels.mtntKMP, TestUtils.LexicalModels.mtnt.fullID))

    wait(for: [installExpectation, groupExpectation], timeout: 5, enforceOrder: true)

    // Step 4:  Verify installation
    XCTAssertTrue(Storage.active.userDefaults.userLexicalModels!.contains { $0.fullID == TestUtils.LexicalModels.mtnt.fullID })
    XCTAssertNotNil(ResourceFileManager.shared.getInstalledPackage(withKey: TestUtils.Packages.Keys.nrc_en_mtnt))
  }

  func testDefaultLexicalModelInstallationClosureTaggedError() throws {
    // Step 1 - mocking
    let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: TestUtils.mockedError)
    mockedURLSession!.queueMockResult(.download(packageDownloadTask))

    let installExpectation = XCTestExpectation(description: "Lexical model package download & installation should complete")
    let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

    // Step 2 - build closure
    let dispatchGroup = DispatchGroup()

    let closure = KeyboardSearchViewController.defaultLexicalModelInstallationClosure(withDownloadManager: downloadManager, dispatchGroup: dispatchGroup) { result in
      if case .error(_) = result {
        // Mocked success!
      } else {
        XCTFail("Lexical model installation did not generated mocked error.")
      }

      installExpectation.fulfill()
    }

    // As the closure has now been built (and thus, DispatchGroup.enter() called),
    // notification only occurs the callback completes.
    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    // Step 3 - run closure
    closure(.tagged(TestUtils.Packages.Keys.nrc_en_mtnt, TestUtils.LexicalModels.mtntKMP, TestUtils.LexicalModels.mtnt.fullID))

    wait(for: [installExpectation, groupExpectation], timeout: 5, enforceOrder: true)

    // Step 4:  Verify lack of installation
    XCTAssertFalse(Storage.active.userDefaults.userLexicalModels?.contains { $0.fullID == TestUtils.LexicalModels.mtnt.fullID } ?? false)
    XCTAssertNil(ResourceFileManager.shared.getInstalledPackage(withKey: TestUtils.Packages.Keys.nrc_en_mtnt))
  }

  func testDefaultLexicalModelInstallationClosureTaggedCancel() throws {
    let installExpectation = XCTestExpectation(description: "Lexical model package download & installation should complete")
    let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

    // Step 2 - build closure
    let dispatchGroup = DispatchGroup()

    let closure = KeyboardSearchViewController.defaultLexicalModelInstallationClosure(withDownloadManager: downloadManager, dispatchGroup: dispatchGroup) { result in
      if case .cancelled = result {
        // Mocked success!
      } else {
        XCTFail("Keyboard-search model cancellation handled improperly.")
      }

      installExpectation.fulfill()
    }

    // As the closure has now been built (and thus, DispatchGroup.enter() called),
    // notification only occurs the callback completes.
    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    // Step 3 - run closure
    closure(.cancelled)

    wait(for: [installExpectation, groupExpectation], timeout: 5, enforceOrder: true)
  }

  func testDeferredLexicalModelSearch() {
    // Step 1:  mocking.
    let packageDownloadTask = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession!.queueMockResult(.download(packageDownloadTask))

    let kbdExpectation = XCTestExpectation(description: "Keyboard package download & installation should complete")
    let lmExpectation = XCTestExpectation(description: "Deferred lexical model search should be triggered")
    let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")

    // Step 2 - build closures & synchronization check
    let dispatchGroup = DispatchGroup()

    let kbdClosure = KeyboardSearchViewController.defaultKeyboardInstallationClosure(withDownloadManager: downloadManager, dispatchGroup: dispatchGroup) { result in
      if case let .success(fullID) = result {
        XCTAssertEqual(fullID as? FullKeyboardID, TestUtils.Keyboards.khmer_angkor.fullID)
      } else {
        XCTFail("keyboard installation did not succeed.")
      }

      kbdExpectation.fulfill()
    }

    // This whole shebang is to mock the "untagged" closure and ensure it is properly called
    // when a language tag is discovered at/after package install, rather than at download time.
    dispatchGroup.enter()
    let lmCallClosure: KeyboardSearchViewController.DeferredLexicalModelSearch = { param in
      switch param {
        case .tag(let tag):
          XCTAssertEqual(tag, "km")
        default:
          XCTFail()
      }

      lmExpectation.fulfill()
      dispatchGroup.leave()
    }

    // As the closure has now been built (and thus, DispatchGroup.enter() called),
    // notification only occurs the callback completes.
    dispatchGroup.notify(queue: .main) {
      groupExpectation.fulfill()
    }

    // Step 3 - run closure
    kbdClosure(.untagged(TestUtils.Packages.Keys.khmer_angkor, TestUtils.Keyboards.khmerAngkorKMP, lmCallClosure))

    wait(for: [kbdExpectation, lmExpectation, groupExpectation], timeout: 5, enforceOrder: true)
  }

  // Synchronization test:  using both closures TOGETHER.
  func testDefaultClosureSynchronization() {
    let kbdExpectation = XCTestExpectation()
    let lmExpectation = XCTestExpectation()
    let groupExpectation = XCTestExpectation(description: "DispatchGroup should notify")
    let dispatchGroup = DispatchGroup()

    let kbdClosure = KeyboardSearchViewController.defaultKeyboardInstallationClosure(withDownloadManager: downloadManager) { _ in kbdExpectation.fulfill() }
    let lmClosure = KeyboardSearchViewController.defaultLexicalModelInstallationClosure(withDownloadManager: downloadManager) { _ in lmExpectation.fulfill() }

    // As the closures have now been built (and thus, DispatchGroup.enter() called),
    // notification only occurs the callback completes.
    dispatchGroup.notify(queue: .main) {
      // Signals that both have completed.
      groupExpectation.fulfill()
    }

    // Step 3 - run closure
    kbdClosure(.cancelled)
    lmClosure(.cancelled)

    // These calls will technically be synchronous, so we can rely on kbd and lm to be
    // in the correct order.  The key is that the groupExpectation is only fulfilled after
    // the other two.
    wait(for: [kbdExpectation, lmExpectation, groupExpectation], timeout: 5, enforceOrder: true)
  }
}
