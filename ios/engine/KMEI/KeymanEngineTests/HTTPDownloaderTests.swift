//
//  HTTPDownloaderTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 5/27/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine

class HTTPDownloaderTests: XCTestCase {
  var downloader: HTTPDownloader?
  var mockedURLSession: TestUtils.Downloading.URLSessionMock?
  
  override func setUp() {
    mockedURLSession = TestUtils.Downloading.URLSessionMock()
    downloader = HTTPDownloader(nil, session: mockedURLSession!)
  }
  
  override func tearDownWithError() throws {
    let queueWasCleared = mockedURLSession!.queueIsEmpty
    mockedURLSession = nil
    downloader = nil
    
    if !queueWasCleared {
      throw NSError(domain: "Keyman",
                    code: 4,
                    userInfo: [NSLocalizedDescriptionKey: "A test did not fully utilize its queued mock results!"])
    }
  }
  
  func testSingleRequestSuccess() throws {
    // Simple, easy case:  our local testing copy of the Khmer Angkor KMP file.
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult))
    
    let testDelegate = TestUtils.Downloading.ExpectationDownloaderDelegate()
    downloader!.handler = testDelegate
    
    let request = HTTPDownloadRequest(url: TestUtils.Keyboards.khmerAngkorKMP)
    downloader!.addRequest(request)
    testDelegate.expect(method: .RequestStarted, request: request)
    testDelegate.expect(method: .RequestFinished, request: request)
    testDelegate.expect(method: .QueueFinished, queue: downloader!)
    
    XCTAssertEqual(downloader!.requestsCount, 1)
    downloader!.run()
    testDelegate.sequentialWait(testCase: self, timeout: 10)
  }
  
  func testSingleRequestFailure() throws {
    // A twist on the previous version - the 'download' fails.
    let mockedResult = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: TestUtils.mockedError)
    mockedURLSession?.queueMockResult(.download(mockedResult))
    
    let testDelegate = TestUtils.Downloading.ExpectationDownloaderDelegate()
    downloader!.handler = testDelegate
    
    let request = HTTPDownloadRequest(url: TestUtils.Keyboards.khmerAngkorKMP)
    downloader!.addRequest(request)
    testDelegate.expect(method: .RequestStarted, request: request)
    testDelegate.expect(method: .RequestFailed, request: request)
    testDelegate.expect(method: .QueueFinished, queue: downloader!)
    
    XCTAssertEqual(downloader!.requestsCount, 1)
    downloader!.run()
    testDelegate.sequentialWait(testCase: self, timeout: 10)
  }
  
  func testSequentialRequests() {
    let mockedResult1 = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult1))
    
    let mockedResult2 = TestUtils.Downloading.MockResult(location:
                                                          TestUtils.LexicalModels.mtntKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult2))
    
    let testDelegate = TestUtils.Downloading.ExpectationDownloaderDelegate()
    downloader!.handler = testDelegate
    
    let request1 = HTTPDownloadRequest(url: TestUtils.Keyboards.khmerAngkorKMP)
    downloader!.addRequest(request1)
    testDelegate.expect(method: .RequestStarted, request: request1)
    testDelegate.expect(method: .RequestFinished, request: request1)
    
    let request2 = HTTPDownloadRequest(url: TestUtils.LexicalModels.mtntKMP)
    downloader!.addRequest(request2)
    testDelegate.expect(method: .RequestStarted, request: request2)
    testDelegate.expect(method: .RequestFinished, request: request2)
    testDelegate.expect(method: .QueueFinished, queue: downloader!)
    
    XCTAssertEqual(downloader!.requestsCount, 2)
    downloader!.run()
    // This is currently safe, as all downloads are sequential.  If we ever
    // enable parallel downloads, this will obviously need to change
    testDelegate.sequentialWait(testCase: self, timeout: 10)
  }
  
  func testRequestCancellation() {
    let mockedResult1 = TestUtils.Downloading.MockResult(location: TestUtils.Keyboards.khmerAngkorKMP, error: nil)
    mockedURLSession?.queueMockResult(.download(mockedResult1))
    
    // No second mocked result - its call should be cancelled.
    
    let testDelegate = TestUtils.Downloading.ExpectationDownloaderDelegate()
    downloader!.handler = testDelegate
    
    let request1 = HTTPDownloadRequest(url: TestUtils.Keyboards.khmerAngkorKMP)
    downloader!.addRequest(request1)
    testDelegate.expect(method: .RequestStarted, request: request1)
    
    let request2 = HTTPDownloadRequest(url: TestUtils.LexicalModels.mtntKMP)
    downloader!.addRequest(request2)
    testDelegate.expectAbsence(method: .RequestStarted, request: request2)
    testDelegate.expectAbsence(method: .RequestFinished, request: request2)
    testDelegate.expect(method: .QueueCancelled, queue: downloader!)
    testDelegate.expectAbsence(method: .QueueFinished, queue: downloader!)
    
    // Placed here for chronological ordering.  We expect the request to finish because of
    // our mocking structure, but want to delay its completion long enough for the
    // `cancelAllOperations` call below to complete.
    testDelegate.expect(method: .RequestFinished, request: request1, completion: { response in
      // Force a delay so that our `cancelAllOperations` call has a chance to execute;
      // our mocking structure is otherwise synchronous!
      sleep(1) // in seconds.
      response.expectation.fulfill()
      return false
    })
    
    // Ideally, we could add this... but because of how things are currently mocked,
    // it actually does trigger - we have a .RequestFinished processing after .QueueCancelled,
    // which then notes the empty queue.
    //
    // testDelegate.expectAbsence(method: .QueueFinished, queue: downloader!)
    
    XCTAssertEqual(downloader!.requestsCount, 2)
    downloader!.run()
    downloader!.cancelAllOperations()
    // This is currently safe, as all downloads are sequential.  If we ever
    // enable parallel downloads, this will obviously need to change
    testDelegate.sequentialWait(testCase: self, timeout: 5)
  }
}
