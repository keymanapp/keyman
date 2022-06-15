//
//  InfoViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit
import Reachability

class InfoViewController: UIViewController, UIWebViewDelegate {
  @IBOutlet var webView: UIWebView!

  @IBOutlet var backButton: UIBarButtonItem!
  @IBOutlet var forwardButton: UIBarButtonItem!

  private var networkReachable: Reachability?

  private var exitClosure: (() -> Void)? = nil;

  convenience init(exitClosure: @escaping () -> Void) {
    if UIDevice.current.userInterfaceIdiom == .phone {
      self.init(nibName: "InfoViewController_iPhone", bundle: nil)
    } else {
      self.init(nibName: "InfoViewController_iPad", bundle: nil)
    }

    self.exitClosure = exitClosure
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    extendedLayoutIncludesOpaqueBars = true
    webView?.delegate = self
    NotificationCenter.default.addObserver(self, selector: #selector(self.networkStatusChanged),
        name: NSNotification.Name.reachabilityChanged, object: nil)

    do {
      try networkReachable = Reachability(hostname: "keyman.com")
      try networkReachable?.startNotifier()
    } catch {
      SentryManager.captureAndLog(error, message: "error starting Reachability notifier: \(error)")
    }
  }

  @objc func networkStatusChanged(_ notification: Notification) {
    reloadKeymanHelp()
  }

  private func updateButtons() {
    backButton.isEnabled = webView.canGoBack
    forwardButton.isEnabled = webView.canGoForward
  }

  func webViewDidStartLoad(_ webView: UIWebView) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = true
    updateButtons()
  }

  func webViewDidFinishLoad(_ webView: UIWebView) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = false
    updateButtons()
  }

  func webView(_ webView: UIWebView, didFailLoadWithError error: Error) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = false
    updateButtons()
    log.debug(error)
  }

  @IBAction func back(_ sender: Any) {
    webView.goBack()
  }

  @IBAction func forward(_ sender: Any) {
    webView.goForward()
  }

  @objc func close(_ sender: Any?) {
    self.exitClosure?()
  }

  func reloadKeymanHelp() {
    loadFromLocal()
  }

  private func loadFromLocal() {
    let offlineHelpBundle = Bundle(path: Bundle.main.path(forResource: "OfflineHelp", ofType: "bundle")!)!

    // Safari won't recognize the contents without the .html ending.
    let filePath = offlineHelpBundle.path(forResource: "index", ofType: "html", inDirectory: nil)
    webView.loadRequest(URLRequest(url: URL.init(fileURLWithPath: filePath!)))
  }

  // Currently unused, as we haven't yet added a toggle to allow users to choose online
  // over the default of offline.
  private func loadFromServer() {
    let appVersion = Version.current.majorMinor
    let url =  "\(KeymanHosts.HELP_KEYMAN_COM)/products/iphone-and-ipad/\(appVersion.plainString)/"
    webView.loadRequest(URLRequest(url: URL(string: url)!))
    log.debug("Info page URL: \(url)")
  }
}
