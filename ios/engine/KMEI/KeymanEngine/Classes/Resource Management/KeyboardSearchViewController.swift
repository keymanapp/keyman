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
public class KeyboardSearchViewController: UIViewController, WKNavigationDelegate {
  public enum SearchDownloadResult {
    case success(KeymanPackage, AnyLanguageResourceFullID)
    case cancelled
    case error(Error?)
  }

  public enum SearchResult<FullID: LanguageResourceFullID> {
    /**
     * Indicates that the user cancelled the search.
     */
    case cancelled

    /**
     * Indicates that a package has been selected for download, but we don't know what language
     * they wish to use it for.  This case will never be specified for the lexical-model selection closure.
     *
     * Once a target language has been identified, call the third parameter's closure to start a search
     * for associated lexical models (or to indicate that none should occur).  Otherwise, the lexical model
     * search closure provided to the keyboard search will never evaluate.
     */
    case untagged(KeymanPackage.Key, URL)

    /**
     * Indicates that a package has been selected for download and is already associated with a
     * language tag.
     */
    case tagged(KeymanPackage.Key, URL, FullID)
  }

  // Useful documentation regarding certain implementation details:
  // https://docs.google.com/document/d/1rhgMeJlCdXCi6ohPb_CuyZd0PZMoSzMqGpv1A8cMFHY/edit

  /**
   * Parameters:
   * 1. The unique package identifier for the selected package.  If nil, the user 'cancelled' and intends for nothing to be installed.
   * 2. The Keyman cloud URL from which that package may be downloaded.  Will be nil if and only if the first parameter is nil.
   * 3. The unique identifier for the resource WITHIN that package to install.  Useful when a package's resource(s) support multiple languages.
   */
  public typealias SelectionCompletedHandler<FullID: LanguageResourceFullID> = (SearchResult<FullID>) -> Void
  public typealias SearchDownloadHandler<FullID: LanguageResourceFullID> = (SearchDownloadResult) -> Void

  private var hasFinalized = false
  private let keyboardSelectionClosure: SelectionCompletedHandler<FullKeyboardID>!
  private let languageCode: String?
  private let session: URLSession

  private static var ENDPOINT_ROOT: URL {
    var baseURL = KeymanHosts.KEYMAN_COM
    baseURL.appendPathComponent("go")
    baseURL.appendPathComponent("ios") // platform
    // API endpoint only supports major.minor, will break if `.build` is also present
    baseURL.appendPathComponent(Version.current.majorMinor.description)
    baseURL.appendPathComponent("download-keyboards")

    return baseURL
  }

  public init(languageCode: String? = nil,
              withSession session: URLSession = URLSession.shared,
              keyboardSelectionBlock: @escaping SelectionCompletedHandler<FullKeyboardID>) {
    self.languageCode = languageCode
    self.session = session
    self.keyboardSelectionClosure = keyboardSelectionBlock
    super.init(nibName: nil, bundle: nil)
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  public override func loadView() {
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
  public func webView(_ webView: WKWebView,
               decidePolicyFor navigationAction: WKNavigationAction,
               decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
    if navigationAction.navigationType == .linkActivated {
      let link = navigationAction.request.url!
      if let parsedLink = UniversalLinks.tryParseKeyboardInstallLink(link) {
        decisionHandler(.cancel)

        // Notify our caller of the search results.
        self.hasFinalized = true  // Prevent popViewController from triggering cancellation events.
        self.navigationController?.popViewController(animated: true) // Rewind UI
        finalize(with: parsedLink)
        return
      } else if UniversalLinks.isExternalLink(link) {
        decisionHandler(.cancel)

        // Kick this link out to an external Safari process.
        UniversalLinks.externalLinkLauncher?(link)
        return
      }
    }

    decisionHandler(.allow)
  }

  override public func viewWillDisappear(_ animated: Bool) {
    // If the view is being dismissed but no matching link has been intercepted...
    if self.isMovingFromParent, !hasFinalized {
      self.keyboardSelectionClosure(.cancelled)
    }
  }

  internal func finalize(with parsedLink: UniversalLinks.ParsedKeyboardInstallLink) {
    let packageKey = KeymanPackage.Key(id: parsedLink.keyboard_id, type: .keyboard)

    // If we have a language ID AND do not yet have a model for it.
    if let lang_id = parsedLink.lang_id {
      let resourceKey = FullKeyboardID(keyboardID: parsedLink.keyboard_id, languageID: lang_id)
      let kbdURL = ResourceDownloadManager.shared.defaultDownloadURL(forPackage: packageKey, andResource: resourceKey, asUpdate: false)
      self.keyboardSelectionClosure(.tagged(packageKey, kbdURL, resourceKey))
    } else {
      let kbdURL = ResourceDownloadManager.shared.defaultDownloadURL(forPackage: packageKey, asUpdate: false)
      self.keyboardSelectionClosure(.untagged(packageKey, kbdURL))
    }
  }

  public static func defaultDownloadClosure(downloadCompletionBlock: @escaping SearchDownloadHandler<FullKeyboardID>) -> SelectionCompletedHandler<FullKeyboardID> {
    return defaultDownloadClosure(withDownloadManager: ResourceDownloadManager.shared,
                                  downloadCompletionBlock: downloadCompletionBlock)
  }

  // For unit testing.
  internal static func defaultDownloadClosure(withDownloadManager downloadManager: ResourceDownloadManager,
                                              downloadCompletionBlock: @escaping SearchDownloadHandler<FullKeyboardID>) -> SelectionCompletedHandler<FullKeyboardID> {
    return { searchResult in
      var packageKey: KeymanPackage.Key
      var packageURL: URL

      switch searchResult {
        case .cancelled:
          downloadCompletionBlock(.cancelled)
          return
        case .untagged(let key, let url):
          packageKey = key
          packageURL = url
        case .tagged(let key, let url, _):
          packageKey = key
          packageURL = url
      }

      let downloadClosure: ResourceDownloadManager.CompletionHandler<KeyboardKeymanPackage> = { package, error in
        guard let package = package, error == nil else {
          downloadCompletionBlock(.error(error))
          return
        }

        switch searchResult {
          case .untagged(_, _):
            let resourceKey = package.installables.first!.first!.fullID
            downloadCompletionBlock(.success(package, resourceKey))
          case .tagged(_, _, let resourceKey):
            downloadCompletionBlock(.success(package, resourceKey))
          default:
            // Illegal state - we already checked this.
            fatalError()
        }
      }

      downloadManager.downloadPackage(withKey: packageKey, from: packageURL, withNotifications: true, completionBlock: downloadClosure)
    }
  }
}
