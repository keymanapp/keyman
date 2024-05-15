//
//  ResourceDownloadManagerTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/29/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class ResourceDownloadManagerTests: XCTestCase {
  var mockedURLSession: TestUtils.Downloading.URLSessionMock?
  var downloadManager: ResourceDownloadManager?
  
  override func setUp() {
    mockedURLSession = TestUtils.Downloading.URLSessionMock()
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: false)
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
  
  func testDefaultKeyboardDownloadURL() {
    let tier = Version.currentTagged.tier ?? .stable
    let root = "/go/package/download"
    
    let basic_khmer_angkor_components = URLComponents(string: downloadManager!.defaultDownloadURL(forPackage: TestUtils.Packages.Keys.khmer_angkor).absoluteString)!
    
    XCTAssertEqual(basic_khmer_angkor_components.path, "\(root)/khmer_angkor")
    XCTAssertTrue(basic_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "platform" && item.value == "ios"
    })
    XCTAssertTrue(basic_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "tier" && item.value == tier.rawValue
    })
    XCTAssertTrue(basic_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "type" && item.value == "keyboard"
    })
    
    let adv_khmer_angkor_url = downloadManager!.defaultDownloadURL(forPackage: TestUtils.Packages.Keys.khmer_angkor, andResource: TestUtils.Keyboards.khmer_angkor.fullID, withVersion: Version("1.0.6"), asUpdate: true)
    let adv_khmer_angkor_components = URLComponents(string: adv_khmer_angkor_url.absoluteString)!
    XCTAssertEqual(adv_khmer_angkor_components.path, "\(root)/khmer_angkor")
    XCTAssertTrue(adv_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "platform" && item.value == "ios"
    })
    XCTAssertTrue(adv_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "tier" && item.value == tier.rawValue
    })
    XCTAssertTrue(adv_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "type" && item.value == "keyboard"
    })
    XCTAssertTrue(adv_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "update" && item.value == "1"
    })
    XCTAssertTrue(adv_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "version" && item.value == "1.0.6"
    })
    XCTAssertTrue(adv_khmer_angkor_components.queryItems!.contains { item in
      return item.name == "bcp47" && item.value == "km"
    })
  }
  
  func testDefaultLexicalModelDownloadURL() {
    let tier = Version.currentTagged.tier ?? .stable
    let root = "/go/package/download"
    
    let basic_mtnt_components = URLComponents(string: downloadManager!.defaultDownloadURL(forPackage: TestUtils.Packages.Keys.nrc_en_mtnt).absoluteString)!
    
    XCTAssertEqual(basic_mtnt_components.path, "\(root)/nrc.en.mtnt")
    XCTAssertTrue(basic_mtnt_components.queryItems!.contains { item in
      return item.name == "platform" && item.value == "ios"
    })
    XCTAssertTrue(basic_mtnt_components.queryItems!.contains { item in
      return item.name == "tier" && item.value == tier.rawValue
    })
    XCTAssertTrue(basic_mtnt_components.queryItems!.contains { item in
      return item.name == "type" && item.value == "model"
    })
    
    let adv_mtnt_url = downloadManager!.defaultDownloadURL(forPackage: TestUtils.Packages.Keys.nrc_en_mtnt, andResource: TestUtils.LexicalModels.mtnt.fullID, withVersion: Version(TestUtils.LexicalModels.mtnt.version), asUpdate: true)
    let adv_mtnt_components = URLComponents(string: adv_mtnt_url.absoluteString)!
    XCTAssertEqual(adv_mtnt_components.path, "\(root)/nrc.en.mtnt")
    XCTAssertTrue(adv_mtnt_components.queryItems!.contains { item in
      return item.name == "platform" && item.value == "ios"
    })
    XCTAssertTrue(adv_mtnt_components.queryItems!.contains { item in
      return item.name == "tier" && item.value == tier.rawValue
    })
    XCTAssertTrue(adv_mtnt_components.queryItems!.contains { item in
      return item.name == "type" && item.value == "model"
    })
    XCTAssertTrue(adv_mtnt_components.queryItems!.contains { item in
      return item.name == "update" && item.value == "1"
    })
    XCTAssertTrue(adv_mtnt_components.queryItems!.contains { item in
      return item.name == "version" && item.value == TestUtils.LexicalModels.mtnt.version
    })
    XCTAssertTrue(adv_mtnt_components.queryItems!.contains { item in
      return item.name == "bcp47" && item.value == "en"
    })
  }
  
  func testDownloadPackageForKeyboard() throws {
    let expectation = XCTestExpectation(description: "Mocked \"download\" should complete successfully.")
    let packageKey = TestUtils.Keyboards.khmer_angkor.packageKey
    
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))
    
    downloadManager?.downloadPackage(withKey: packageKey, from: TestUtils.Keyboards.khmerAngkorKMP, withNotifications: false) { (package: KeyboardKeymanPackage?, error: Error?) in
      
      let tempDownloadKMP = ResourceFileManager.shared.packageDownloadTempPath(forKey: packageKey)
      XCTAssertFalse(FileManager.default.fileExists(atPath: tempDownloadKMP.path))
      
      // Assertions:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let cachedDocumentsKMP = ResourceFileManager.shared.cachedPackagePath(forKey: packageKey)
      
      XCTAssertTrue(FileManager.default.fileExists(atPath: cachedDocumentsKMP.path))
      
      XCTAssertNil(error)
      XCTAssertNotNil(package)
      
      // Basic verification that it really is the khmer_angkor keyboard package.
      if let package = package {
        XCTAssertEqual(package.id, TestUtils.Keyboards.khmer_angkor.id)
        XCTAssertNotNil(package.findResource(withID: TestUtils.Keyboards.khmer_angkor.fullID))
      }
      
      expectation.fulfill()
    }
    
    let downloadQueue = downloadManager!.downloader
    downloadQueue.step()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testDownloadPackageForLexicalModel() throws {
    let expectation = XCTestExpectation(description: "Mocked \"download\" should complete successfully.")
    let packageKey = TestUtils.LexicalModels.mtnt.packageKey
    
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))
    
    downloadManager?.downloadPackage(withKey: packageKey, from: TestUtils.LexicalModels.mtntKMP, withNotifications: false) { (package: LexicalModelKeymanPackage?, error: Error?) in
      let tempDownloadKMP = ResourceFileManager.shared.packageDownloadTempPath(forKey: packageKey)
      XCTAssertFalse(FileManager.default.fileExists(atPath: tempDownloadKMP.path))
      
      // Assertion:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let cachedDocumentsKMP = ResourceFileManager.shared.cachedPackagePath(forKey: packageKey)
      
      XCTAssertTrue(FileManager.default.fileExists(atPath: cachedDocumentsKMP.path))
      
      XCTAssertNil(error)
      XCTAssertNotNil(package)
      
      // Basic verification that it really is the khmer_angkor keyboard package.
      if let package = package {
        XCTAssertEqual(package.id, TestUtils.LexicalModels.mtnt.id)
        XCTAssertNotNil(package.findResource(withID: TestUtils.LexicalModels.mtnt.fullID))
      }
      
      expectation.fulfill()
    }
    
    let downloadQueue = downloadManager!.downloader
    downloadQueue.step()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testDownloadPackageFailure() throws {
    let expectation = XCTestExpectation(description: "Mocked \"download\" should complete, though with an error.")
    let packageKey = TestUtils.Keyboards.khmer_angkor.packageKey
    
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: TestUtils.mockedError)
    mockedURLSession?.queueMockResult(.download(mockedResult))
    
    downloadManager?.downloadPackage(withKey: packageKey, from: TestUtils.Keyboards.khmerAngkorKMP, withNotifications: false) { package, error in
      
      let tempDownloadKMP = ResourceFileManager.shared.packageDownloadTempPath(forKey: packageKey)
      XCTAssertFalse(FileManager.default.fileExists(atPath: tempDownloadKMP.path))
      
      // Assertion:  a copy of the KMP file exists in the documents directory, facilitating user sharing.
      let cachedDocumentsKMP = ResourceFileManager.shared.cachedPackagePath(forKey: packageKey)
      
      // On download failure, we shouldn't be producing a file here.
      XCTAssertFalse(FileManager.default.fileExists(atPath: cachedDocumentsKMP.path))
      
      XCTAssertNotNil(error)
      XCTAssertNil(package)
      
      expectation.fulfill()
    }
    
    let downloadQueue = downloadManager!.downloader
    downloadQueue.step()
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testLegacyDownloadKeyboard() throws {
    let expectation = XCTestExpectation(description: "Mocked package \"download\" should complete successfully.")
    
    // For this test, we actually want the downloader to run automatically.  It's... a bit tricky to
    // run this integration test otherwise.
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: true)
    
    let mockedDownload = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    
    mockedURLSession?.queueMockResult(.download(mockedDownload))
    
    let khmer_angkor_id = TestUtils.Keyboards.khmer_angkor.fullID
    
    downloadManager?.downloadKeyboard(withID: khmer_angkor_id.id, languageID: khmer_angkor_id.languageID, isUpdate: false, fetchRepositoryIfNeeded: false) { package, error in
      if let error = error {
        XCTFail("Mocked query and download should both succeed; reported error: \(error.localizedDescription)")
      } else if let package = package {
        // Double-check it!
        XCTAssertEqual(package.id, "khmer_angkor")
        XCTAssertNotNil(package.findResource(withID: khmer_angkor_id))
      }
      
      expectation.fulfill()
    }
    
    wait(for: [expectation], timeout: 5)
  }
  
  func testLegacyDownloadLexicalModel() throws {
    let expectation = XCTestExpectation(description: "Mocked package \"download\" should complete successfully.")
    
    // For this test, we actually want the downloader to run automatically.  It's... a bit tricky to
    // run this integration test otherwise.
    downloadManager = ResourceDownloadManager(session: mockedURLSession!, autoExecute: true)
    
    let mockedDownload = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)
    
    mockedURLSession?.queueMockResult(.download(mockedDownload))
    
    let mtnt_id = TestUtils.LexicalModels.mtnt.fullID
    
    downloadManager?.downloadLexicalModel(withID: mtnt_id.id, languageID: mtnt_id.languageID, isUpdate: false, fetchRepositoryIfNeeded: false) { package, error in
      if let error = error {
        XCTFail("Mocked query and download should both succeed; reported error: \(error.localizedDescription)")
      } else if let package = package {
        // Double-check it!
        XCTAssertEqual(package.id, "nrc.en.mtnt")
        XCTAssertNotNil(package.findResource(withID: mtnt_id))
      }
      
      expectation.fulfill()
    }
    
    wait(for: [expectation], timeout: 5)
  }
  
  // This annotation prevents local deprecation warnings for what it tests.
  @available(*, deprecated, message: "This method tests the deprecated ResourceDownloadManager.stateForKeyboard.")
  func testStateForKeyboard() {
    let baseInstallation = XCTestExpectation(description: "Mocked \"download\" should complete successfully.")
    let khmer_angkor_id = TestUtils.Keyboards.khmer_angkor.fullID
    let packageKey = TestUtils.Keyboards.khmer_angkor.packageKey
    
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))
    
    XCTAssertEqual(downloadManager!.stateForKeyboard(withID: khmer_angkor_id.id), .needsDownload)
    
    downloadManager!.downloadPackage(withKey: packageKey,
                                     from: TestUtils.Keyboards.khmerAngkorKMP,
                                     withNotifications: false) { (package: KeyboardKeymanPackage?, error) in
      if let _ = error {
        XCTFail()
        baseInstallation.fulfill()
        return
      } else if let package = package {
        do {
          try ResourceFileManager.shared.install(resourceWithID: khmer_angkor_id, from: package)
          baseInstallation.fulfill()
        } catch {
          XCTFail()
          baseInstallation.fulfill()
        }
      }
    }
    
    XCTAssertEqual(downloadManager!.stateForKeyboard(withID: khmer_angkor_id.id), .downloading)
    
    downloadManager!.downloader.step()
    
    wait(for: [baseInstallation], timeout: 5)
    
    XCTAssertEqual(downloadManager!.stateForKeyboard(withID: khmer_angkor_id.id), .upToDate)
    
    // Now, to test the update-check part of the function...
    // This fixture was hand-altered.  At the time of this test's creation, this version did not exist.
    let mockedQuery = TestUtils.Downloading.MockResult(location: TestUtils.Queries.package_version_km_updated, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedQuery))
    
    let versionQuery = XCTestExpectation()
    
    // Now to do a package-version check.
    KeymanPackage.queryCurrentVersions(for: [KeymanPackage.Key(id: khmer_angkor_id.id, type: .keyboard)], withSession: downloadManager!.session) { result, _ in
      XCTAssertEqual(self.downloadManager!.stateForKeyboard(withID: khmer_angkor_id.id), .needsUpdate)
      versionQuery.fulfill()
    }
    
    wait(for: [versionQuery], timeout: 5)
  }
  
  public func testDefaultLexicalModelForLanguage() {
    let mockedQueryWithModel = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_en, error: nil)
    let mockedModelDownload = TestUtils.Downloading.MockResult(location: TestUtils.LexicalModels.mtntKMP, error: nil)
    let mockedQueryWithoutModel = TestUtils.Downloading.MockResult(location: TestUtils.Queries.model_case_km, error: nil)
    mockedURLSession?.queueMockResult(.data(mockedQueryWithModel))
    mockedURLSession?.queueMockResult(.download(mockedModelDownload))
    mockedURLSession?.queueMockResult(.data(mockedQueryWithoutModel))
    
    let foundExpectation = XCTestExpectation()
    let noneExpectation = XCTestExpectation()
    
    downloadManager?.downloader.autoExecute = true
    downloadManager?.downloadLexicalModelsForLanguageIfExists(languageID: "en") { package, error in
      if error != nil {
        XCTFail()
      }
      
      XCTAssertNotNil(package)
      
      foundExpectation.fulfill()
    }
    
    wait(for: [foundExpectation], timeout: 5)
    
    downloadManager?.downloadLexicalModelsForLanguageIfExists(languageID: "km") { package, error in
      if error != nil {
        XCTFail()
      }
      
      XCTAssertNil(package)
      
      noneExpectation.fulfill()
    }
    
    wait(for: [noneExpectation], timeout: 5)
  }
}
