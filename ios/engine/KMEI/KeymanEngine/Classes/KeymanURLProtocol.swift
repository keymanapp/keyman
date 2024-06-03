//
//  KeymanURLProtocol.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation
import os.log

class KeymanURLProtocol: URLProtocol, NSURLConnectionDataDelegate {
  private static let protocolHandledKey = "KMURLProtocolHandledKey"
  private var connection: NSURLConnection?

  // MARK: - URLProtocol
  override class func canInit(with request: URLRequest) -> Bool {
    let mainDoc = request.mainDocumentURL?.lastPathComponent
    let scheme = request.url?.scheme
    if mainDoc == "keyboard.html" && scheme != "file" {
      return URLProtocol.property(forKey: protocolHandledKey, in: request) == nil
    }
    return false
  }

  override class func canonicalRequest(for request: URLRequest) -> URLRequest {
    return request
  }

  override class func requestIsCacheEquivalent(_ a: URLRequest, to b: URLRequest) -> Bool {
    return super.requestIsCacheEquivalent(a, to: b)
  }

  override func startLoading() {
    guard let mutableRequest = request as? NSMutableURLRequest else {
      os_log("Bridge to NSMutableURLRequest failed", log:KeymanEngineLogger.resources, type: .error)
      return
    }
    URLProtocol.setProperty(true, forKey: KeymanURLProtocol.protocolHandledKey, in: mutableRequest)
  }

  override func stopLoading() {
    connection?.cancel()
    connection = nil
  }

  // MARK: - NSURLConnectionDataDelegate
  func connection(_ connection: NSURLConnection, didReceive response: URLResponse) {
    client?.urlProtocol(self, didReceive: response, cacheStoragePolicy: .notAllowed)
  }

  func connection(_ connection: NSURLConnection, didReceive data: Data) {
    client?.urlProtocol(self, didLoad: data)
  }

  func connectionDidFinishLoading(_ connection: NSURLConnection) {
    client?.urlProtocolDidFinishLoading(self)
  }

  func connection(_ connection: NSURLConnection, didFailWithError error: Error) {
    client?.urlProtocol(self, didFailWithError: error)
  }
}
