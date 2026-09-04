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
  
  /**
   * update the view when SwiftUI state changes
   */
  public func updateNSView(_ nsView: WKWebView, context: Context) {
    // first update the parent in the coordinator so we know which
    // package we are displaying content for
    context.coordinator.parent = self
    
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
    Coordinator(self)
  }
  
  @MainActor
  public class Coordinator: NSObject, WKNavigationDelegate {
    var parent: PackageContentWebView
    
    init(_ parent: PackageContentWebView) {
      self.parent = parent
    }
    
    /**
     * If a url links to the web rather than locally, open it in the default browser
     * Must force load links in the package content that change the main frame
     * because WKWebView's security sandbox will block loading of new pages
     */
    public func webView(_ webView: WKWebView,
                        decidePolicyFor navigationAction: WKNavigationAction,
                        decisionHandler: @escaping @MainActor @Sendable (WKNavigationActionPolicy) -> Void) {
      
      // if no url, cancel
      guard let url = navigationAction.request.url else {
        decisionHandler(.cancel)
        return
      }
      
      // if not user-activated, pass through, e.g. for redirects
      guard navigationAction.navigationType == .linkActivated else {
        print("url not user activated \(url.path())")
        decisionHandler(.allow)
        return
      }
      
      // load local files in webview, force-loading files that are in the same directory
      if url.isFileURL {
        let standardizedIncomingUrl = url.standardizedFileURL
        let packageDirectoryUrl = parent.packageFileUrl.deletingLastPathComponent()
        let standardizedPackageDirectoryUrl = packageDirectoryUrl.standardizedFileURL
        
        // check whether the file is from the current package
        if standardizedIncomingUrl.path.hasPrefix(standardizedPackageDirectoryUrl.path) {
          decisionHandler(.cancel) // Cancel regular navigation
          
          // Force load with read permissions of package directory
          webView.loadFileURL(standardizedIncomingUrl, allowingReadAccessTo: standardizedPackageDirectoryUrl)
          return
        } else {
          // block local files outside of your target directory, not sure how we would receive one
          decisionHandler(.cancel)
          return
        }
      }
      
      // not a file URL: handle external links by opening in web browser
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
