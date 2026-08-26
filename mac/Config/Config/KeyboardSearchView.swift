/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-16
 *
 * Webview to search for Keyman keyboards/packages
 */

import Foundation

import SwiftUI
import Combine
import WebKit
import KeymanSettings

struct KeyboardSearchView: NSViewRepresentable {
  @ObservedObject var coordinator: DownloadCoordinator
  @EnvironmentObject var settings: SettingsContainer
  
  // note that the EnvironmentObject is not available within init (if we were to implement that)
  // it is injected just before makeNSView and updateNSView are called

  /** Creates the underlying NSView (WKWebView) for macOS */
  func makeNSView(context: Context) -> WKWebView {
    print("makeNSView called")
    let webView = WKWebView()
    
    // assign the coordinator as the navigation delegate
    webView.navigationDelegate = self.coordinator
    
    let request = URLRequest(url: settings.keyboardSearchUrl)
    webView.load(request)
    return webView
  }
  
  /**
   * Updates the view when the state changes.
   * This is a safe place to pass the SettingsContainer to the Coordinator
   * as the environment has been loaded by now.
   */
  func updateNSView(_ nsView: WKWebView, context: Context) {
    if coordinator.settings == nil {
      coordinator.settings = self.settings
      print("updateNSView, settings intialized for coordinator")
    }
  }
}


