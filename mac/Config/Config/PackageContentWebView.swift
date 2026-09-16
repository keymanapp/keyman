/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-08-03
 *
 * Webview used to display html content from within the Keyman package
 * Any http links clicked are opened in a browser window
 */
import Foundation

import SwiftUI
import WebKit
import KeymanSettings

public struct PackageContentWebView: NSViewRepresentable {
  let packageFileUrl: URL
  
  // create the AppKit view instance
  public func makeNSView(context: Context) -> WKWebView {
    let webView = WKWebView()
    
    // Connect the delegate to catch link clicks
    webView.navigationDelegate = context.coordinator

    return webView
  }
  
  // update the view when SwiftUI state changes
  public func updateNSView(_ nsView: WKWebView, context: Context) {
    let request = URLRequest(url: packageFileUrl)
    
    // only load the request if it's not already loading/loaded to prevent infinite loops
    if nsView.url != packageFileUrl {
      if let fileUrl = request.url {
        nsView.loadFileURL(fileUrl, allowingReadAccessTo: fileUrl.deletingLastPathComponent())
      }
    }
  }
  
  /**
   * Coordinator acts as the WKNavigationDelegate
   */
  public func makeCoordinator() -> Coordinator {
    Coordinator()
  }
  
  /**
   * If a url links to the web rather than locally, open it in the default browser
   */
  @MainActor
  public class Coordinator: NSObject, WKNavigationDelegate {
    public func webView(_ webView: WKWebView,
                        decidePolicyFor navigationAction: WKNavigationAction,
                        decisionHandler: @escaping @MainActor @Sendable (WKNavigationActionPolicy) -> Void) {

      // if not url, cancel
      guard let url = navigationAction.request.url else {
        decisionHandler(.cancel)
        return
      }
      
      // if not user-activated, pass through, e.g. for redirects
      guard navigationAction.navigationType == .linkActivated else {
          decisionHandler(.allow)
          return
      }

      // local files load in webview
      if url.isFileURL {
          decisionHandler(.allow)
          return
      }

      // handle external links by opening in web browser
      decisionHandler(.cancel)

      var externalUrl = url

      // strip "link:" prefix if present
      let urlString = url.absoluteString
      if urlString.hasPrefix("link:https://") || urlString.hasPrefix("link:http://") {
          let cleanString = urlString.replacingOccurrences(of: "link:", with: "")
          if let cleanUrl = URL(string: cleanString) {
              externalUrl = cleanUrl
          }
      }

      // Open the external link in the default browser
      NSWorkspace.shared.open(externalUrl)
    }
  }
}
