//
//  URLSessionMock.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 5/27/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import XCTest

extension TestUtils.Downloading {
  class MockResult {
    let location: URL?
    let error: Error?
    let statusCode: Int

    init(location: URL?, error: Error?, statusCode: Int = 200) {
      self.location = location
      self.error = error
      self.statusCode = statusCode
    }
  }

  /**
   * Many thanks to https://www.swiftbysundell.com/articles/mocking-in-swift/ for the approach used here.
   */
  class URLSessionMock: URLSession {
    enum Task {
      case data(MockResult)
      case download(MockResult)
    }

    enum URLSessionMockError: Error {
      case unexpectedTaskType
      case mockedFileNotFound
      case dataInitializationError
      case mockQueueEmpty
    }

    private var mockedResultQueue: [(Task, XCTestExpectation?)]

    override init() {
      mockedResultQueue = []
    }

    func queueMockResult(_ result: Task, expectation: XCTestExpectation? = nil) {
      mockedResultQueue.append((result, expectation))
    }

    var queueIsEmpty: Bool {
      get {
        return mockedResultQueue.isEmpty
      }
    }

    override func downloadTask(with url: URL, completionHandler: @escaping (URL?, URLResponse?, Error?) -> Void) -> URLSessionDownloadTask {

      if(mockedResultQueue.count <= 0) {
        return URLSessionDownloadTaskMock {
          completionHandler(nil, nil, URLSessionMockError.mockQueueEmpty)
        }
      } else {
        if case (.download(let response), let expectation) = self.mockedResultQueue.removeFirst() {
          let mockedURLResponse = HTTPURLResponse(url: response.location!, statusCode: 200, httpVersion: nil, headerFields: nil)
          return URLSessionDownloadTaskMock(response: mockedURLResponse) {
            completionHandler(response.location, mockedURLResponse, response.error)
            expectation?.fulfill()
          }
        } else {
          return URLSessionDownloadTaskMock {
            completionHandler(nil, nil, URLSessionMockError.unexpectedTaskType)
          }
        }
      }
    }

    override func dataTask(with url: URL, completionHandler: @escaping (Data?, URLResponse?, Error?) -> Void) -> URLSessionDataTask {
      if(mockedResultQueue.count <= 0) {
        return URLSessionDataTaskMock {
          completionHandler(nil, nil, URLSessionMockError.mockQueueEmpty)
        }
      } else {
        return URLSessionDataTaskMock {
          if case (.data(let response), let expectation) = self.mockedResultQueue.removeFirst() {
            if !FileManager.default.fileExists(atPath: response.location!.path) {
              // TODO:  Properly mock HTTPURLSession for 404 errors?
              completionHandler(nil, nil, URLSessionMockError.mockedFileNotFound)
              expectation?.fulfill()
              return
            }

            do {
              let data = try Data(contentsOf: response.location!, options: .mappedIfSafe)
              if(data.isEmpty) {
                completionHandler(nil, nil, URLSessionMockError.dataInitializationError)
              }

              // A bit of white-box testing - we know that HTTPDownloader doesn't actually examine
              // the URLResponse headers, so we don't provide any.
              completionHandler(data, nil, response.error)
            } catch {
              completionHandler(nil, nil, URLSessionMockError.dataInitializationError)
            }
            expectation?.fulfill()
          } else {
            completionHandler(nil, nil, URLSessionMockError.unexpectedTaskType)
          }
        }
      }
    }
  }
}
