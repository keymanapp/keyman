//
//  URLSessionDataTaskMock.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/26/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

extension TestUtils.Downloading {
  /**
   * Many thanks to https://www.swiftbysundell.com/articles/mocking-in-swift/ for the approach used here.
   */
  class URLSessionDataTaskMock: URLSessionDataTask {
    private let closure: () -> Void
    
    init(closure: @escaping () -> Void) {
      self.closure = closure
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
