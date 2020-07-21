//
//  KeyboardSearchViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 7/21/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import UIKit
import WebKit

/*
 * Minor design notes:
 *
 * This class is designed for use as a 'library component' of KeymanEngine that apps may choose
 * whether or not to utilize.  It simply returns the specifications of any packages/resources
 * to download, leaving control of their actual installation up to KeymanEngine's host app.
 */

/**
 * Loads a WebView to Keyman's online keyboard search.  This WebView will intercept any package download links
 * and return the specifications of such interceptions - the search results - to the instance's owner via a closure specified
 * during initialization.
 */
class KeyboardSearchViewController: UIViewController, WKNavigationDelegate {
  // Useful documentation regarding certain implementation details:
  // https://docs.google.com/document/d/1rhgMeJlCdXCi6ohPb_CuyZd0PZMoSzMqGpv1A8cMFHY/edit

  typealias SearchCompletionHandler = (KeymanPackage.Key?, FullKeyboardID?) -> Void

  private let completionClosure: SearchCompletionHandler!
  private let languageCode: String?

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

  public init(languageCode: String? = nil, searchCompletionBlock: @escaping SearchCompletionHandler) {
    self.languageCode = languageCode
    self.completionClosure = searchCompletionBlock
    super.init(nibName: nil, bundle: nil)
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func loadView() {
    let webView = WKWebView()
    webView.navigationDelegate = self
    if let languageCode = languageCode {
      let baseURL = KeyboardSearchViewController.ENDPOINT_ROOT
      let specificURL = URL.init(string: "\(baseURL)?q=l:id:\(languageCode)")!
      webView.load(URLRequest(url: specificURL))
    } else {
      webView.load(URLRequest(url: KeyboardSearchViewController.ENDPOINT_ROOT))
    }

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


        if match.numberOfRanges > 2, let lang_id_range = Range(match.range(at: 2), in: linkString) {
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

  public static func defaultResultInstallationClosure() -> SearchCompletionHandler {
    return { packageKey, resourceKey in
      if let packageKey = packageKey {
        let url = ResourceDownloadManager.shared.defaultDownloadURL(forPackage: packageKey,
                                                                    andResource: resourceKey,
                                                                    asUpdate: false)

        var closure: ResourceDownloadManager.CompletionHandler<KeyboardKeymanPackage>

        if let resourceKey = resourceKey {
          closure = ResourceDownloadManager.shared.standardKeyboardInstallCompletionBlock(forFullID: resourceKey)
        } else {
          closure = { package, error in
            guard package == nil || error != nil else {
              // TODO:  Show a proper alert
              return
            }

            // TODO:  We don't know which resource the user actually wants.  Prompt them.
          }
        }

        ResourceDownloadManager.shared.downloadPackage(withKey: packageKey, from: url, completionBlock: closure)
      }
    }
  }
}
