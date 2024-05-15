//
//  BaseTestDownloaderDelegate.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 5/28/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import XCTest
@testable import KeymanEngine

extension TestUtils.Downloading {
  class ExpectationDownloaderDelegate: HTTPDownloadDelegate {
    enum Method: String {
      case RequestStarted
      case RequestFinished
      case RequestFailed
      case QueueFinished
      case QueueCancelled
    }
    
    typealias ExpectationMetBlock = (ExpectationQueueEntry) -> Bool
    
    struct ExpectationQueueEntry {
      let method: Method
      let request: HTTPDownloadRequest?
      let queue: HTTPDownloader?
      let completion: ExpectationMetBlock?
      let expectation: XCTestExpectation
      
      init(method: Method, request: HTTPDownloadRequest, completion: ExpectationMetBlock? = nil) {
        self.init(method: method, request: request, queue: nil, completion: completion)
      }
      
      init(method: Method, queue: HTTPDownloader, completion: ExpectationMetBlock? = nil) {
        self.init(method: method, request: nil, queue: queue, completion: completion)
      }
      
      private init(method: Method, request: HTTPDownloadRequest?, queue: HTTPDownloader?, completion: ExpectationMetBlock? = nil) {
        self.method = method
        self.request = request
        self.queue = queue
        self.completion = completion
        
        // Now to dynamically build the description test for the Expectation, then the Expectation itself
        var description: String
        switch method {
        case .RequestStarted, .RequestFinished, .RequestFailed:
          description = "\(method.rawValue) for \(request!.typeCode.rawValue) from \(request!.url)"
        case .QueueFinished, .QueueCancelled:
          description = "\(method.rawValue)"
        }
        self.expectation = XCTestExpectation(description: description)
      }
    }
    
    private var expectationQueue: [ExpectationQueueEntry] = []
    private var expectedAbsences: [ExpectationQueueEntry] = []
    
    private func doExpectationMatch(method: Method, request: HTTPDownloadRequest? = nil, queue: HTTPDownloader? = nil) {
      
      if expectationQueue.count == 0 {
        return
      } else {
        let head = expectationQueue[0]
        if head.method == method && head.request == request && head.queue == queue {
          expectationQueue.removeFirst()
          
          // Now to look to fulfill the expectation.
          if head.completion != nil {
            if head.completion!(head) {
              head.expectation.fulfill()
            }
          } else {
            head.expectation.fulfill()
          }
        }
      }
    }
    
    func downloadRequestStarted(_ request: HTTPDownloadRequest) {
      doExpectationMatch(method: .RequestStarted, request: request)
    }
    
    func downloadRequestFinished(_ request: HTTPDownloadRequest) {
      doExpectationMatch(method: .RequestFinished, request: request)
    }
    
    func downloadRequestFailed(_ request: HTTPDownloadRequest, with error: Error?) {
      doExpectationMatch(method: .RequestFailed, request: request)
    }
    
    func downloadQueueFinished(_ queue: HTTPDownloader) {
      doExpectationMatch(method: .QueueFinished, queue: queue)
    }
    
    func downloadQueueCancelled(_ queue: HTTPDownloader) {
      doExpectationMatch(method: .QueueCancelled, queue: queue)
    }
    
    func expect(method: Method, request: HTTPDownloadRequest, completion: ExpectationMetBlock? = nil) {
      expectationQueue.append(ExpectationQueueEntry(method: method, request: request, completion: completion))
    }
    
    func expect(method: Method, queue: HTTPDownloader, completion: ExpectationMetBlock? = nil) {
      expectationQueue.append(ExpectationQueueEntry(method: method, queue: queue, completion: completion))
    }
    
    func expectAbsence(method: Method, request: HTTPDownloadRequest, completion: ExpectationMetBlock? = nil) {
      let entry = ExpectationQueueEntry(method: method, request: request, completion: completion)
      entry.expectation.isInverted = true
      expectedAbsences.append(entry)
    }
    
    func expectAbsence(method: Method, queue: HTTPDownloader, completion: ExpectationMetBlock? = nil) {
      let entry = ExpectationQueueEntry(method: method, queue: queue, completion: completion)
      entry.expectation.isInverted = true
      expectedAbsences.append(entry)
    }
    
    func sequentialWait(testCase: XCTestCase, timeout: TimeInterval = 10) {
      //      while expectationQueue.count > 0 {
      //        testCase.wait(for: [expectationQueue[0].expectation], timeout: timeout)
      //      }
      var expectationList: [XCTestExpectation] = expectationQueue.map { entry in
        return entry.expectation
      }
      
      let absenceList: [XCTestExpectation] = expectedAbsences.map { entry in
        return entry.expectation
      }
      
      expectationList.append(contentsOf: absenceList)
      testCase.wait(for: expectationList, timeout: timeout, enforceOrder: true)
    }
  }
}
