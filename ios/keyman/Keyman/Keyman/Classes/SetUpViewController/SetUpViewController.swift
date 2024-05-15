//
//  SetUpViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import WebKit
import Reachability
import os

// TODO: Refactor common functionality from InfoViewController
class SetUpViewController: UIViewController, WKNavigationDelegate {
  @IBOutlet var webView: WKWebView!
  
  private var networkReachable: Reachability?
  
  convenience init() {
    self.init(nibName: "SetUpViewController", bundle: nil)
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()
    
    let navBar = view.viewWithTag(786586) as? UINavigationBar
    let doneButton = navBar?.topItem?.rightBarButtonItem
    doneButton?.target = self
    doneButton?.action = #selector(self.dismissView)
    webView?.navigationDelegate = self
    NotificationCenter.default.addObserver(self, selector: #selector(self.networkStatusChanged),
                                           name: NSNotification.Name.reachabilityChanged, object: nil)
    
    do {
      try networkReachable = Reachability(hostname: "keyman.com")
      try networkReachable?.startNotifier()
    } catch {
      let message = "error thrown starting Reachability notifier:  \(error)"
      os_log("%{public}s", log:KeymanLogger.ui, type: .error, message)
      SentryManager.capture(error, message: message)
    }
  }
  
  @objc func networkStatusChanged(_ notification: Notification) {
    reloadKeymanHelp()
  }
  
  func reloadKeymanHelp() {
    if let networkStatus = networkReachable?.connection {
      switch networkStatus {
      case Reachability.Connection.none, Reachability.Connection.unavailable:
        loadFromLocal()
      default:
        loadFromServer()
      }
    } else {
      loadFromLocal()
    }
  }
  
  @objc func dismissView() {
    dismiss(animated: true, completion: nil)
  }
  
  private func loadFromLocal() {
    let offlineHelpBundle = Bundle(path: Bundle.main.path(forResource: "OfflineHelp", ofType: "bundle")!)!
    
    let filePath = offlineHelpBundle.path(forResource: "installing-system-keyboard",
                                          ofType: "html",
                                          inDirectory: "start")
    webView.load(URLRequest(url: URL.init(fileURLWithPath: filePath!)))
  }
  
  private func loadFromServer() {
    let appVersion = Version.current.majorMinor
    let url = "\(KeymanHosts.HELP_KEYMAN_COM)/products/iphone-and-ipad/\(appVersion.plainString)"
    + "/start/installing-system-keyboard?embed=ios"
    webView.load(URLRequest(url: URL(string: url)!))
    os_log("Set up page URL: %{public}s", log: KeymanLogger.resources, type: .debug, url)
  }
}
