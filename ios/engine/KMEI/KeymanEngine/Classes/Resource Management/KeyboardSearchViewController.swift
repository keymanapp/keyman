//
//  KeyboardSearchViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 7/21/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import UIKit
import WebKit

class KeyboardSearchViewController: UIViewController, WKNavigationDelegate {
  // Useful documentation regarding certain implementation details:
  // https://docs.google.com/document/d/1rhgMeJlCdXCi6ohPb_CuyZd0PZMoSzMqGpv1A8cMFHY/edit

  typealias SearchCompletionHandler = (KeymanPackage.Key?, FullKeyboardID?) -> Void

  private let completionClosure: SearchCompletionHandler!

  private static var ENDPOINT_ROOT: URL {
    var baseURL = KeymanHosts.KEYMAN_COM
    baseURL.appendPathComponent("go")
    baseURL.appendPathComponent("ios") // platform
    // API endpoint only supports major.minor, will break if `.build` is also present
    baseURL.appendPathComponent(Version.current.majorMinor.description)
    baseURL.appendPathComponent("download-keyboards")

    return baseURL
  }

  private let REGEX_FOR_DOWNLOAD_INTERCEPT = try! NSRegularExpression(pattern: "^http(?:s)?:\\/\\/[^\\/]+\\/keyboards\\/install\\/([^?\\/]+)(?:\\?(.+))?$")

  public init(searchCompletionBlock: @escaping SearchCompletionHandler) {
    self.completionClosure = searchCompletionBlock
    super.init(nibName: nil, bundle: nil)
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func loadView() {
    let webView = WKWebView()
    webView.navigationDelegate = self
    webView.load(URLRequest(url: KeyboardSearchViewController.ENDPOINT_ROOT))

    view = webView
  }

  // Used to intercept download links.
  func webView(_ webView: WKWebView,
               decidePolicyFor navigationAction: WKNavigationAction,
               decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
    if navigationAction.navigationType == .linkActivated {
      let link = navigationAction.request.url!
      let linkString = link.absoluteString

      // If it matches the format for the Keyboard Universal Link URL Pattern...
      // (see https://docs.google.com/document/d/1rhgMeJlCdXCi6ohPb_CuyZd0PZMoSzMqGpv1A8cMFHY/edit?ts=5f11cb13#heading=h.qw7pas2adckj)
      if let match = REGEX_FOR_DOWNLOAD_INTERCEPT.firstMatch(in: linkString,
                                                             options: [],
                                                             range: NSRange(location: 0, length: link.absoluteString.utf16.count)) {
        let keyboard_id_range = Range(match.range(at: 1), in: linkString)!
        let keyboard_id = String(linkString[keyboard_id_range])

        let packageKey = KeymanPackage.Key(id: keyboard_id, type: .keyboard)
        var resourceKey: FullKeyboardID? = nil

        if match.numberOfRanges > 2 {
          let lang_id_range = Range(match.range(at: 2), in: linkString)!
          let lang_id = String(linkString[lang_id_range])

          resourceKey = FullKeyboardID(keyboardID: keyboard_id, languageID: lang_id)
        }

        decisionHandler(.cancel)

        // Notify our caller of the search results.
        self.navigationController?.popViewController(animated: true) // Rewind UI
        self.completionClosure(packageKey, resourceKey)
        return
      }
    }

    decisionHandler(.allow)
  }
}
