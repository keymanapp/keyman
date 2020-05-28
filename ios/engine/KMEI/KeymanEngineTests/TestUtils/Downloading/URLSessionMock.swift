//
//  URLSessionMock.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 5/27/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

extension TestUtils.Downloading {
  class DownloadMockResult {
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
    private var mockedResultQueue: [DownloadMockResult]

    override init() {
      mockedResultQueue = []
    }

    func queueMockResult(_ result: DownloadMockResult) {
      mockedResultQueue.append(result)
    }

    var queueIsEmpty: Bool {
      get {
        return mockedResultQueue.isEmpty
      }
    }

    override func downloadTask(with url: URL, completionHandler: @escaping (URL?, URLResponse?, Error?) -> Void) -> URLSessionDownloadTask {

      if(mockedResultQueue.count <= 0) {
        fatalError("No mocked results have been provided for this downloadTask call!")
      } else {
        return URLSessionDownloadTaskMock {
          let response = self.mockedResultQueue.removeFirst()

          // A bit of white-box testing - we know that HTTPDownloader doesn't actually examine
          // the URLResponse headers, so we don't provide any.
          completionHandler(response.location, nil, response.error)
        }
      }
    }
  }
}
