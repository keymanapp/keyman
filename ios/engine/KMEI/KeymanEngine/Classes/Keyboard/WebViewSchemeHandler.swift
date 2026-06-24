/*
 *  Keyman is copyright (C) SIL Global. MIT License.
 *
 *  Created by Joshua Horton on 6/24/26.
 *
 *  WebViewKeyboardLoader implements a URLSchemeHandler that allows
 *  the hosted Keyman Engine for Web to access all files, consistently,
 *  via a http-like protocol, preventing CORS access issues for files
 *  loaded dynamically.
 */

import WebKit
import UniformTypeIdentifiers
import os.log

func getMimeType(forExtension ext: String) -> String {
    // Find the UTType associated with the file extension
    if let utType = UTType(filenameExtension: ext) {
        // Return the preferred MIME type if it exists
        return utType.preferredMIMEType ?? "application/octet-stream"
    }
    return "application/octet-stream"
}

class WebViewSchemeHandler: NSObject, WKURLSchemeHandler {
  let storage: Storage
  let scheme = "keyman-engine"
  
  init(storage: Storage) {
    self.storage = storage
  }
  
  func webView(_ webView: WKWebView, start urlSchemeTask: any WKURLSchemeTask) {
    guard let url = urlSchemeTask.request.url else {
      return
    }
    
    var components = URLComponents(url: url, resolvingAgainstBaseURL: false)!
    components.scheme = "file"
  
    let fileUrl = components.url
    
    let doError = { () -> Void in
      let message = "Could not load url via WKURLSchemeHandler: \(url)"
      let errorInfo = [
        NSLocalizedDescriptionKey: message
      ]
      let error = NSError(domain: "WebViewKeyboardLoader", code: 500, userInfo: errorInfo)
      
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(error, message: message)
      
      urlSchemeTask.didFailWithError(error)
    }
    
    guard fileUrl != nil else {
      doError()
      return
    }
    
    do {
      let fileContents = try Data(contentsOf: fileUrl!)
      let fileExtension = fileUrl!.pathExtension
      
      let mimeType: String = getMimeType(forExtension: fileExtension)
      let charset: String = mimeType.hasPrefix("text/") ? "; charset=utf-8" : ""
      
      let response = HTTPURLResponse(
        url: url,
        statusCode: 200,
        httpVersion: "HTTP/1.1",
        headerFields: [
          "Content-Type": "\(mimeType)\(charset)",
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
    var loadingURLBuilder = URLComponents()
    loadingURLBuilder.scheme = scheme
    loadingURLBuilder.path = fileURL.path
    return loadingURLBuilder.url!
  }
}
