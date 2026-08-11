/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-08-03
 *
 * Webview used to show help for Keyman keyboards
 */
import Foundation

import SwiftUI
import WebKit
import KeymanSettings

public struct HelpView: NSViewRepresentable {
  @EnvironmentObject var settings: SettingsContainer
  
  let helpFileURL: URL
  
  // create the AppKit view instance
  public func makeNSView(context: Context) -> WKWebView {
    let webView = WKWebView()
    return webView
  }
  
  // update the view when SwiftUI state changes
  public func updateNSView(_ nsView: WKWebView, context: Context) {
    let request = URLRequest(url: helpFileURL)
    
    // only load the request if it's not already loading/loaded to prevent infinite loops
    if nsView.url != helpFileURL {
      nsView.load(request)
    }
  }
}
