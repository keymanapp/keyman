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
  public enum DefaultInstallationResult {
    case success(AnyLanguageResourceFullID)
    case cancelled
    case error(Error?)
  }

  public enum DelayedLanguageSelection {
    case none
    case tag(String)
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
    case untagged(KeymanPackage.Key, URL, (DelayedLanguageSelection) -> Void)

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
  internal typealias DeferredLexicalModelSearch = (DelayedLanguageSelection) -> Void

  private let keyboardSelectionClosure: SelectionCompletedHandler<FullKeyboardID>!
  private let lexicalModelSelectionClosure: SelectionCompletedHandler<FullLexicalModelID>!
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

  private static let REGEX_FOR_DOWNLOAD_INTERCEPT = try! NSRegularExpression(pattern: "^http(?:s)?:\\/\\/[^\\/]+\\/keyboards\\/install\\/([^?\\/]+)(?:\\?(.+))?$")

  public init(languageCode: String? = nil,
              withSession session: URLSession = URLSession.shared,
              keyboardSelectionBlock: @escaping SelectionCompletedHandler<FullKeyboardID>,
              lexicalModelSelectionBlock: @escaping SelectionCompletedHandler<FullLexicalModelID>) {
    self.languageCode = languageCode
    self.session = session
    self.keyboardSelectionClosure = keyboardSelectionBlock
    self.lexicalModelSelectionClosure = lexicalModelSelectionBlock
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
      if let (keyboard_id, lang_id) = KeyboardSearchViewController.tryParseLink(link) {
        decisionHandler(.cancel)

        // Notify our caller of the search results.
        self.navigationController?.popViewController(animated: true) // Rewind UI
        finalize(with: keyboard_id, for: lang_id)
        return
      }
    }

    decisionHandler(.allow)
  }

  internal static func tryParseLink(_ link: URL) -> (String, String?)? {
    let linkString = link.absoluteString

    // If it matches the format for the Keyboard Universal Link URL Pattern...
    // (see https://docs.google.com/document/d/1rhgMeJlCdXCi6ohPb_CuyZd0PZMoSzMqGpv1A8cMFHY/edit?ts=5f11cb13#heading=h.qw7pas2adckj)
    if let match = REGEX_FOR_DOWNLOAD_INTERCEPT.firstMatch(in: linkString,
                                                           options: [],
                                                           range: NSRange(location: 0, length: linkString.utf16.count)) {
      let keyboard_id_range = Range(match.range(at: 1), in: linkString)!
      let keyboard_id = String(linkString[keyboard_id_range])

      var lang_id: String? = nil
      let urlComponents = URLComponents(string: linkString)!
      if let lang_id_component = urlComponents.queryItems?.first(where: { $0.name == "bcp47" }) {
        lang_id = lang_id_component.value
      }

      return (keyboard_id, lang_id)
    } else {
      return nil
    }
  }

  internal func finalize(with keyboard_id: String, for lang_id: String?) {
    let packageKey = KeymanPackage.Key(id: keyboard_id, type: .keyboard)

    // If we have a language ID AND do not yet have a model for it.
    if let lang_id = lang_id {
      let resourceKey = FullKeyboardID(keyboardID: keyboard_id, languageID: lang_id)
      let kbdURL = ResourceDownloadManager.shared.defaultDownloadURL(forPackage: packageKey, andResource: resourceKey, asUpdate: false)
      performLanguageSearch(languageID: lang_id)
      self.keyboardSelectionClosure(.tagged(packageKey, kbdURL, resourceKey))
    } else {
      // No language ID?  Defer the language search!
      let deferredModelClosure: DeferredLexicalModelSearch = { arg in
        if case let .tag(lgCode) = arg {
          self.performLanguageSearch(languageID: lgCode)
        } else {
          self.lexicalModelSelectionClosure(.cancelled)
        }
      }
      let kbdURL = ResourceDownloadManager.shared.defaultDownloadURL(forPackage: packageKey, asUpdate: false)
      self.keyboardSelectionClosure(.untagged(packageKey, kbdURL, deferredModelClosure))
    }
  }

  internal func performLanguageSearch(languageID: String) {
    if Storage.active.userDefaults.userLexicalModels?.contains(where: { $0.languageID == languageID }) == nil {
      Queries.LexicalModel.fetchModels(forLanguageCode: languageID,
                                       withSession: session) { results, error in
        if let results = results {
          if results.count == 0 {
            self.lexicalModelSelectionClosure(.cancelled)
          } else {
            let lexicalModel = results[0].0
            self.lexicalModelSelectionClosure(.tagged(lexicalModel.packageKey, results[0].1, lexicalModel.fullID))
          }
        } else {
          // .cancelled because if we already errored once, a redo of the query is likely
          // to raise an error again.  We -could- defer as if there were no language tag, though.
          self.lexicalModelSelectionClosure(.cancelled)

          if let error = error {
            log.error("Could not find a lexical model for language id \"\(languageID)\" due to error: \(String(describing: error))")
          }
        }
      }
    } else {
      // We already have a lexical model for the language.  No need to download a new one.
      self.lexicalModelSelectionClosure(.cancelled)
    }
  }


  public static func defaultKeyboardInstallationClosure(installCompletionBlock: ((DefaultInstallationResult) -> Void)? = nil) -> SelectionCompletedHandler<FullKeyboardID> {
    return defaultKeyboardInstallationClosure(withDownloadManager: ResourceDownloadManager.shared,
                                              installCompletionBlock: installCompletionBlock)
  }

  // For unit testing.
  internal static func defaultKeyboardInstallationClosure(withDownloadManager downloadManager: ResourceDownloadManager,
                                                          dispatchGroup: DispatchGroup? = nil,
                                                          installCompletionBlock: ((DefaultInstallationResult) -> Void)? = nil) -> SelectionCompletedHandler<FullKeyboardID> {
    dispatchGroup?.enter() // register the closure for group synchronization.

    // Used to finalize the results of the closure, allowing the callback to complete
    // before signaling 'group completion' to the DispatchGroup synchronization object.
    func finalize(as result: DefaultInstallationResult, noCallbackLog message: String? = nil) {
      if let installCompletionBlock = installCompletionBlock {
        installCompletionBlock(result)
      } else if let message = message {
        log.error(message)
      }

      dispatchGroup?.leave() // "fulfill" this closure's aspect of the group's synchronization scheme.
    }

    return { searchResult in
      var packageKey: KeymanPackage.Key
      var packageURL: URL

      switch searchResult {
        case .cancelled:
          finalize(as: .cancelled)
          return
        case .untagged(let key, let url, _):
          packageKey = key
          packageURL = url
        case .tagged(let key, let url, _):
          packageKey = key
          packageURL = url
      }

      let downloadClosure: ResourceDownloadManager.CompletionHandler<KeyboardKeymanPackage> = { package, error in
        guard let package = package, error == nil else {
          let errString = error != nil ? String(describing: error) : "<unknown error>"
          let message = "Could not download package \(packageKey): \(errString)"

          finalize(as: .error(error), noCallbackLog: message)
          if case let .untagged(_, _, deferredLexicalModelSearch) = searchResult {
            deferredLexicalModelSearch(.none)
          }
          return
        }

        switch searchResult {
          case .untagged(_, _, let deferredLexicalModelSearch):
            do {
              // TODO:  We don't know which resource the user actually wants.  Prompt them.
              // But for now, the old 'default' installation.
              try ResourceFileManager.shared.finalizePackageInstall(package, isCustom: false)
              // Whatever solution we put in place should return (at least) one of these:
              let resourceKey = package.installables.first!.first!.fullID

              // Yeah, it's ugly... but it's best to fix as part of resolution of the TODO above.
              // That's "the first language pairing of the first keyboard in the package."
              finalize(as: .success(resourceKey))
              deferredLexicalModelSearch(.tag(resourceKey.languageID))
            } catch {
              finalize(as: .error(error), noCallbackLog: "Could not install package \(packageKey): \(error)")
              deferredLexicalModelSearch(.none)
              throw error // For more accurate notifications
            }
          case .tagged(_, _, let resourceKey):
            do {
              try ResourceFileManager.shared.install(resourceWithID: resourceKey, from: package)
              finalize(as: .success(resourceKey))
            } catch {
              finalize(as: .error(error), noCallbackLog: "Could not install package \(packageKey): \(error)")
              throw error // For more accurate notifications
            }
          default:
            // Illegal state - we already checked this.
            fatalError()
        }
      }

      downloadManager.downloadPackage(withKey: packageKey, from: packageURL, withNotifications: true, completionBlock: downloadClosure)
    }
  }

  public static func defaultLexicalModelInstallationClosure(installCompletionBlock: ((DefaultInstallationResult) -> Void)? = nil) -> SelectionCompletedHandler<FullLexicalModelID> {
    return defaultLexicalModelInstallationClosure(withDownloadManager: ResourceDownloadManager.shared)
  }

  // For unit testing.
  internal static func defaultLexicalModelInstallationClosure(withDownloadManager downloadManager: ResourceDownloadManager,
                                                              dispatchGroup: DispatchGroup? = nil,
                                                              installCompletionBlock: ((DefaultInstallationResult) -> Void)? = nil) -> SelectionCompletedHandler<FullLexicalModelID> {
    dispatchGroup?.enter() // register the closure for group synchronization.

    // Used to finalize the results of the closure, allowing the callback to complete
    // before signaling 'group completion' to the DispatchGroup synchronization object.
    func finalize(as result: DefaultInstallationResult, noCallbackLog message: String? = nil) {
      if let installCompletionBlock = installCompletionBlock {
        installCompletionBlock(result)
      } else if let message = message {
        log.error(message)
      }

      dispatchGroup?.leave() // "fulfill" this closure's aspect of the group's synchronization scheme.
    }

    return { searchResult in
      switch searchResult {
        case .cancelled:
          finalize(as: .cancelled)
          return
        case .tagged(let packageKey, let packageURL, let resourceKey):
          var lmInstallClosure: ResourceDownloadManager.CompletionHandler<LexicalModelKeymanPackage>
          lmInstallClosure = { package, error in
            if let package = package, error == nil {
              //  perform the actual installation
              do {
                try ResourceFileManager.shared.install(resourceWithID: resourceKey, from: package)
                finalize(as: .success(resourceKey))
              } catch {
                finalize(as: .error(error), noCallbackLog: "Could not install \(resourceKey) from package \(packageKey): \(error)")
                throw error
              }
            } else {
              let errString = error != nil ? String(describing: error) : "<unknown error>"
              let message = "Could not download package \(packageKey): \(errString)"

              finalize(as: .error(error), noCallbackLog: message)
              throw error ?? NSError()
            }
          }

          downloadManager.downloadPackage(withKey: packageKey, from: packageURL, withNotifications: true, completionBlock: lmInstallClosure)
        case .untagged(_, _, _):
          fatalError() // Explicitly illegal state, as documented in the enum's definition.
      }
    }
  }
}
