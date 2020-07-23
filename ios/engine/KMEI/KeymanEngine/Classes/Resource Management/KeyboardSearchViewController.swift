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
  enum DefaultInstallationResult {
    case success(AnyLanguageResourceFullID)
    case cancelled
    case error(Error?)
  }

  // Useful documentation regarding certain implementation details:
  // https://docs.google.com/document/d/1rhgMeJlCdXCi6ohPb_CuyZd0PZMoSzMqGpv1A8cMFHY/edit

  typealias SelectionCompletedHandler<FullID: LanguageResourceFullID> = (KeymanPackage.Key?, FullID?) -> Void

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

  private let REGEX_FOR_DOWNLOAD_INTERCEPT = try! NSRegularExpression(pattern: "^http(?:s)?:\\/\\/[^\\/]+\\/keyboards\\/install\\/([^?\\/]+)(?:\\?(.+))?$")

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

          Queries.LexicalModel.fetchModels(forLanguageCode: resourceKey!.languageID,
                                           withSession: session) { results, error in
            if let results = results {
              if results.count == 0 {
                self.lexicalModelSelectionClosure(nil, nil)
              } else {
                let lexicalModel = results[0].0
                self.lexicalModelSelectionClosure(lexicalModel.packageKey, lexicalModel.fullID)
              }
            } else {
              self.lexicalModelSelectionClosure(nil, nil)

              if let error = error {
                log.error("Could not find a lexical model for language id \"\(lang_id)\" due to error: \(String(describing: error))")
              }
            }
          }
        }

        decisionHandler(.cancel)

        // Notify our caller of the search results.
        self.navigationController?.popViewController(animated: true) // Rewind UI
        self.keyboardSelectionClosure(packageKey, resourceKey)
        return
      }
    }

    decisionHandler(.allow)
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

    return { packageKey, resourceKey in
      if let packageKey = packageKey {
        let url = downloadManager.defaultDownloadURL(forPackage: packageKey,
                                                                 andResource: resourceKey,
                                                                 asUpdate: false)
        let downloadClosure: ResourceDownloadManager.CompletionHandler<KeyboardKeymanPackage> = { package, error in
          guard let package = package, error == nil else {
            let errString = error != nil ? String(describing: error) : "<unknown error>"
            let message = "Could not download package \(packageKey): \(errString)"

            finalize(as: .error(error), noCallbackLog: message)
            return
          }

          do {
            if let resourceKey = resourceKey {
              try ResourceFileManager.shared.install(resourceWithID: resourceKey, from: package)
              finalize(as: .success(resourceKey))
            } else {
              // TODO:  We don't know which resource the user actually wants.  Prompt them.
              // But for now, the old 'default' installation.
              try ResourceFileManager.shared.finalizePackageInstall(package, isCustom: false)
              // Yeah, it's ugly... but it's best to fix as part of resolution of the TODO above.
              // That's "the first language pairing of the first keyboard in the package."
              finalize(as: .success(package.installables.first!.first!.fullID))
            }
          } catch {
            finalize(as: .error(error), noCallbackLog: "Could not install package \(packageKey): \(error)")
          }
        }

        downloadManager.downloadPackage(withKey: packageKey, from: url, completionBlock: downloadClosure)
      } else {
        finalize(as: .cancelled)
      }
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

    return { packageKey, resourceKey in
      if let packageKey = packageKey, let resourceKey = resourceKey {
        // TODO:  Does not currently work properly for lexical models.
        //        That said, there are plans to remedy this.
        let url = downloadManager.defaultDownloadURL(forPackage: packageKey,
                                                                 andResource: resourceKey,
                                                                 asUpdate: false)

        var lmInstallClosure: ResourceDownloadManager.CompletionHandler<LexicalModelKeymanPackage>
        lmInstallClosure = { package, error in
          if let package = package, error == nil {
            //  perform the actual installation
            do {
              try ResourceFileManager.shared.install(resourceWithID: resourceKey, from: package)
              finalize(as: .success(resourceKey))
            } catch {
              finalize(as: .error(error), noCallbackLog: "Could not install \(resourceKey) from package \(packageKey): \(error)")
            }
          } else {
            let errString = error != nil ? String(describing: error) : "<unknown error>"
            let message = "Could not download package \(packageKey): \(errString)"

            finalize(as: .error(error), noCallbackLog: message)
            return
          }
        }

        downloadManager.downloadPackage(withKey: packageKey, from: url, completionBlock: lmInstallClosure)
      } else {
        finalize(as: .cancelled)
      }
    }
  }
}
