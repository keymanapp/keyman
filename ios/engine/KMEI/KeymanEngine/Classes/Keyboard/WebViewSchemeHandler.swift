/*
 *  Keyman is copyright (C) SIL Global. MIT License.
 *
 *  Created by Joshua Horton on 2026-06-24.
 */

import WebKit
import UniformTypeIdentifiers
import os.log

func getMimeType(forExtension ext: String) -> String {
    if let utType = UTType(filenameExtension: ext) {
        return utType.preferredMIMEType ?? "application/octet-stream"
    }
    return "application/octet-stream"
}

class WebViewSchemeHandler: NSObject, WKURLSchemeHandler {
  let storage: Storage
  let scheme = "keyman-engine"
  let appGroupIdentifier = "group.com.dal.keyman.keysense"
  
  init(storage: Storage) {
    self.storage = storage
  }
  
  func webView(_ webView: WKWebView, start urlSchemeTask: any WKURLSchemeTask) {
    guard let rawUrl = urlSchemeTask.request.url else { return }
    
    // Step 1: Strip query parameters (e.g., ?v=1789873996447)
    var urlComponents = URLComponents(url: rawUrl, resolvingAgainstBaseURL: false)
    urlComponents?.query = nil
    var urlString = urlComponents?.url?.absoluteString ?? rawUrl.absoluteString
    
    // Step 2: Extract path after the final scheme definition
    if let lastSchemeRange = urlString.range(of: "\(scheme):", options: .backwards) {
      urlString = String(urlString[lastSchemeRange.upperBound...])
    }
    
    // Step 3: Strip leading slashes
    while urlString.hasPrefix("/") {
      urlString = String(urlString.dropFirst())
    }
    
    var fileUrl = URL(fileURLWithPath: "/" + urlString)
    
    // Step 4: Map path to Shared App Group Container
    if let sharedContainer = FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: appGroupIdentifier) {
      let fileName = fileUrl.lastPathComponent
      
      // First check direct path in shared container
      let sharedPathUrl = sharedContainer.appendingPathComponent(urlString)
      if FileManager.default.fileExists(atPath: sharedPathUrl.path) {
        fileUrl = sharedPathUrl
      } else {
        // Look in Library/ and subdirectories inside the App Group
        let sharedLibraryUrl = sharedContainer.appendingPathComponent("Library/\(fileName)")
        let sharedRootUrl = sharedContainer.appendingPathComponent(fileName)
        
        if FileManager.default.fileExists(atPath: sharedLibraryUrl.path) {
          fileUrl = sharedLibraryUrl
        } else if FileManager.default.fileExists(atPath: sharedRootUrl.path) {
          fileUrl = sharedRootUrl
        }
      }
    }
    
    let doError = { () -> Void in
      let message = "Could not load url via WKURLSchemeHandler in Extension: \(rawUrl)"
      let errorInfo = [NSLocalizedDescriptionKey: message]
      let error = NSError(domain: "WebViewKeyboardLoader", code: 500, userInfo: errorInfo)
      
      os_log("%{public}s", log: KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
      
      urlSchemeTask.didFailWithError(error)
    }
    
    do {
      let fileContents = try Data(contentsOf: fileUrl)
      let fileExtension = fileUrl.pathExtension
      
      let mimeType: String = fileExtension == "css" ? "text/css" : getMimeType(forExtension: fileExtension)
      let charset: String = (mimeType.hasPrefix("text/") || mimeType == "application/json")
        ? "; charset=utf-8"
        : ""
      
      let response = HTTPURLResponse(
        url: rawUrl,
        statusCode: 200,
        httpVersion: "HTTP/1.1",
        headerFields: [
          "Content-Type": "\(mimeType)\(charset)",
          "Access-Control-Allow-Origin": "*"
        ]
      )!
      
      urlSchemeTask.didReceive(response)
      urlSchemeTask.didReceive(fileContents)
      urlSchemeTask.didFinish()
    } catch {
      doError()
      return
    }
  }
  
  func webView(_ webView: WKWebView, stop urlSchemeTask: any WKURLSchemeTask) {
  }
  
  func buildUrlForFile(fileURL: URL) -> URL {
    var cleanPath = fileURL.path
    if let lastSchemeRange = cleanPath.range(of: "\(scheme):", options: .backwards) {
      cleanPath = String(cleanPath[lastSchemeRange.upperBound...])
    }
    while cleanPath.hasPrefix("/") {
      cleanPath = String(cleanPath.dropFirst())
    }
    return URL(string: "\(scheme):/\(cleanPath)")!
  }
}
