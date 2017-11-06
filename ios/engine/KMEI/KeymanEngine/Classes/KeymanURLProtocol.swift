//
//  KeymanURLProtocol.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

public class KeymanURLProtocol: URLProtocol, NSURLConnectionDataDelegate {
  private static let protocolHandledKey = "KMURLProtocolHandledKey"
  private var connection: NSURLConnection?

  // MARK: - URLProtocol
  public override class func canInit(with request: URLRequest) -> Bool {
    let mainDoc = request.mainDocumentURL?.lastPathComponent
    let scheme = request.url?.scheme
    if mainDoc == "keyboard.html" && scheme != "file" {
      return URLProtocol.property(forKey: protocolHandledKey, in: request) == nil
    }
    return false
  }

  public override class func canonicalRequest(for request: URLRequest) -> URLRequest {
    return request
  }

  public override class func requestIsCacheEquivalent(_ a: URLRequest, to b: URLRequest) -> Bool {
    return super.requestIsCacheEquivalent(a, to: b)
  }

  public override func startLoading() {
    guard let mutableRequest = request as? NSMutableURLRequest else {
      Manager.shared.kmLog("KeymanURLProtocol: Bridge to NSMutableURLRequest failed",
                                       checkDebugPrinting: true)
      return
    }
    URLProtocol.setProperty(true, forKey: KeymanURLProtocol.protocolHandledKey, in: mutableRequest)
  }

  public override func stopLoading() {
    connection?.cancel()
    connection = nil
  }

  // MARK: - NSURLConnectionDataDelegate
  public func connection(_ connection: NSURLConnection, didReceive response: URLResponse) {
    client?.urlProtocol(self, didReceive: response, cacheStoragePolicy: .notAllowed)
  }

  public func connection(_ connection: NSURLConnection, didReceive data: Data) {
    client?.urlProtocol(self, didLoad: data)
  }

  public func connectionDidFinishLoading(_ connection: NSURLConnection) {
    client?.urlProtocolDidFinishLoading(self)
  }

  public func connection(_ connection: NSURLConnection, didFailWithError error: Error) {
    client?.urlProtocol(self, didFailWithError: error)
  }
}
