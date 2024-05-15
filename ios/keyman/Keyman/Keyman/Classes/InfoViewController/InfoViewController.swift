//
//  InfoViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import WebKit
import Reachability
import os

class InfoViewController: UIViewController, WKNavigationDelegate {
  @IBOutlet var webView: WKWebView!
  
  @IBOutlet var backButton: UIBarButtonItem!
  @IBOutlet var forwardButton: UIBarButtonItem!
  
  private var networkReachable: Reachability?
  
  private var exitClosure: (() -> Void)?
  
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
    webView?.navigationDelegate = self
    NotificationCenter.default.addObserver(self, selector: #selector(self.networkStatusChanged),
                                           name: NSNotification.Name.reachabilityChanged, object: nil)
    
    do {
      try networkReachable = Reachability(hostname: "keyman.com")
      try networkReachable?.startNotifier()
    } catch {
      let message = "error starting Reachability notifier: \(error)"
      os_log("%{public}s", log:KeymanLogger.ui, type: .error, message)
      SentryManager.capture(error, message: message)
    }
  }
  
  @objc func networkStatusChanged(_ notification: Notification) {
    reloadKeymanHelp()
  }
  
  private func updateButtons() {
    backButton.isEnabled = webView.canGoBack
    forwardButton.isEnabled = webView.canGoForward
  }
  
  func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = true
    updateButtons()
  }
  
  func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = false
    updateButtons()
  }
  
  func webView(_ webView: WKWebView, didFailNavigation error: Error) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = false
    updateButtons()
    os_log("%{public}s", log: KeymanLogger.ui, type: .error, error.localizedDescription)
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
    webView.load(URLRequest(url: URL.init(fileURLWithPath: filePath!)))
  }
  
  // Currently unused, as we haven't yet added a toggle to allow users to choose online
  // over the default of offline.
  private func loadFromServer() {
    let appVersion = Version.current.majorMinor
    let url =  "\(KeymanHosts.HELP_KEYMAN_COM)/products/iphone-and-ipad/\(appVersion.plainString)/"
    webView.load(URLRequest(url: URL(string: url)!))
    os_log("Info page URL: %{public}s", log: KeymanLogger.ui, type: .debug, url)
  }
}
