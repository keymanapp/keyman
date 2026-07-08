/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-16
 *
 * Webview to search for Keyman keyboards
 */

import Foundation

import SwiftUI
import WebKit
import KeymanSettings

struct KeyboardSearchView: NSViewRepresentable {
  @EnvironmentObject var settings: SettingsContainer
  
  // note that the EnvironmentObject is not available within init (if we were to implement that)
  // it is injected just before makeNSView and updateNSView are called
  
  // MAC-CONFIG-TODO: build URL rather than hard-code
  let searchURL = URL(string: "https://keyman.com/go/macos/14.0/download-keyboards/?version=19.0.284")!

  /** Creates the Coordinator to handle WebKit delegate methods */
  func makeCoordinator() -> Coordinator {
    Coordinator()
  }
  
  /** Creates the underlying NSView (WKWebView) for macOS */
  func makeNSView(context: Context) -> WKWebView {
    print("makeNSView called")
    let webView = WKWebView()
    
    // assign the coordinator as the navigation delegate
    webView.navigationDelegate = context.coordinator
    
    let request = URLRequest(url: searchURL)
    webView.load(request)
    return webView
  }
  
  /**
   * Updates the view when the state changes.
   * This is a safe place to pass the SettingsContainer to the Coordinator
   * as the environment has been loaded by now.
   */
  func updateNSView(_ nsView: WKWebView, context: Context) {
    if context.coordinator.settings == nil {
      context.coordinator.settings = self.settings
      print("updateNSView, settings intialized for coordinator")
    }
  }
  
  class Coordinator: NSObject, WKNavigationDelegate, WKDownloadDelegate {
    var downloadFileUrl: URL? = nil
    var settings: SettingsContainer?
    
    func webView(_ webView: WKWebView,
                 decidePolicyFor navigationAction: WKNavigationAction,
                 preferences: WKWebpagePreferences,
                 decisionHandler: @escaping @MainActor (WKNavigationActionPolicy, WKWebpagePreferences) -> Void) {
      
      print("deciding navigation based on action")
      
      if let url = navigationAction.request.url {
        print("webView navigationAction.request.url: \(url)")
      }
      
      // Trust HTML download attribute if present
      if navigationAction.shouldPerformDownload {
        print("webView called decisionHandler for download")
        decisionHandler(.download, preferences)
        return
      }
      
      // MAC-CONFIG-TODO: is this necessary or is download attribute enough to identify
      // check if URL ends with a target file extension
      if let url = navigationAction.request.url {
        if url.pathExtension.lowercased() == KeymanPaths.keymanPackageFileExtension {
          decisionHandler(.download, preferences)
          print("webView found .kmp, called decisionHandler for download")
          return
        }
      }
      
      decisionHandler(.allow, preferences)
    }
    
    /** decide whether the navigation should be allowed, canceled or result in a download */
    func webView(_ webView: WKWebView,
                 decidePolicyFor navigationResponse: WKNavigationResponse,
                 decisionHandler: @escaping @MainActor (WKNavigationResponsePolicy) -> Void) {
      print("deciding navigation based on response")
      
      if navigationResponse.canShowMIMEType {
        decisionHandler(.allow)
      } else {
        guard let keymanSettings = self.settings else {
          print("webView decidePolicyFor:decisionHandler: no settings")
          decisionHandler(.cancel)
          return
        }
        
        // if a download is already in progress then stop another from starting
        if keymanSettings.isDownloadInProgress() {
          print("download already in progress, download canceled")
          decisionHandler(.cancel)
        } else {
          decisionHandler(.download)
        }
      }
    }
    
    func webView(_ webView: WKWebView, navigationAction: WKNavigationAction, didBecome download: WKDownload) {
      print("webView navigationAction:didBecome called")
      download.delegate = self // Assign delegate for file saving
    }
    
    func webView(_ webView: WKWebView,
                 navigationResponse: WKNavigationResponse,
                 didBecome download: WKDownload) {
      print("webView navigationResponse:didBecome called")
      download.delegate = self
    }

    func download(_ download: WKDownload, decideDestinationUsing response: URLResponse, suggestedFilename: String, completionHandler: @escaping @MainActor @Sendable (URL?) -> Void) {
      print("download initiated")
      
      guard let keymanSettings = self.settings else {
        print("tried to access settings before they were intialized in updateNSView")
        completionHandler(nil)
        return
      }
      
      // notify settings that a keyboard download is beginning and get the URL to
      // the temporary folder where it should be downloaded
      
      downloadFileUrl = keymanSettings.preparePackageDownload(kmpFileName: suggestedFilename)
      if let downloadFileUrl {
        completionHandler(downloadFileUrl)
      } else {
        print("could not prepare package for download")
        completionHandler(nil)
      }
    }
    
    func downloadDidFinish(_ download: WKDownload) {
      if let downloadFileUrl {
        print("Download of \(downloadFileUrl.path()) was successful.")
        if let settings {
          settings.packageDownloadComplete(kmpFileUrl: downloadFileUrl)
        }
      }
    }
    
    // MAC-CONFIG-TODO: remove package if it already exists

    func download(_ download: WKDownload, didFailWithError error: Error, resumeData: Data?) {
      print("Download failed with error: \(error.localizedDescription)")
    }

    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
        // The web process crashed. Reload the webview safely here.
        print("WebKit process terminated unexpectedly: reloading content...")
        webView.reload()
    }
  }
}


