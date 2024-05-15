//
//  URLSessionDownloadTaskMock.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 5/27/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

extension TestUtils.Downloading {
  /**
   * Many thanks to https://www.swiftbysundell.com/articles/mocking-in-swift/ for the approach used here.
   */
  class URLSessionDownloadTaskMock: URLSessionDownloadTask {
    private let closure: () -> Void
    private let _response: URLResponse?
    
    init(response: URLResponse? = nil, closure: @escaping () -> Void) {
      self.closure = closure
      self._response = response
    }
    
    public override var response: URLResponse? {
      return _response
    }
    
    /*
     * For mocked DownloadTasks, just use the precomputed completion closure
     * provided to the constructor.
     */
    override func resume() {
      closure()
    }
    
    override func cancel() {
      // We must prevent the original cancel() from occurring, as it'll
      // cause asynchronous errors otherwise.
    }
  }
}
