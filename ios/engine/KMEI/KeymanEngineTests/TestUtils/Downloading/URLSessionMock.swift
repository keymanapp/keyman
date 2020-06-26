//
//  URLSessionMock.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 5/27/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import XCGLogger

extension TestUtils.Downloading {
  class MockResult {
    let location: URL?
    let error: Error?

    init(location: URL?, error: Error?) {
      self.location = location
      self.error = error
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

    private var mockedResultQueue: [Task]

    override init() {
      mockedResultQueue = []
    }

    func queueMockResult(_ result: Task) {
      mockedResultQueue.append(result)
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
        return URLSessionDownloadTaskMock {
          if case .download(let response) = self.mockedResultQueue.removeFirst() {
            // A bit of white-box testing - we know that HTTPDownloader doesn't actually examine
            // the URLResponse headers, so we don't provide any.
            completionHandler(response.location, nil, response.error)
          } else {
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
          if case .data(let response) = self.mockedResultQueue.removeFirst() {
            if !FileManager.default.fileExists(atPath: response.location!.path) {
              // TODO:  Properly mock HTTPURLSession for 404 errors?
              completionHandler(nil, nil, URLSessionMockError.mockedFileNotFound)
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
          } else {
            completionHandler(nil, nil, URLSessionMockError.unexpectedTaskType)
          }
        }
      }
    }
  }
}
