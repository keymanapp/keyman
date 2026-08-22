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
      
      // check whether the user clicked a link
      if navigationAction.navigationType == .linkActivated,
         let url = navigationAction.request.url {
        
        // if it is an external link, intercept it and open it a browser window
        if url.scheme == "http" || url.scheme == "https" {
          NSWorkspace.shared.open(url) // opens default macOS browser
          decisionHandler(.cancel)     // blocks the webview from loading it
          return
        }
      }
      
      // allow local navigation
      decisionHandler(.allow)
    }
  }
}
