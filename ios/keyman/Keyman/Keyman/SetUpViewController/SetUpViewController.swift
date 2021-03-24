//
//  SetUpViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit
import Reachability

// TODO: Refactor common functionality from InfoViewController
class SetUpViewController: UIViewController, UIWebViewDelegate {
  @IBOutlet var webView: UIWebView!

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
    webView?.delegate = self
    NotificationCenter.default.addObserver(self, selector: #selector(self.networkStatusChanged),
        name: NSNotification.Name.reachabilityChanged, object: nil)

    do {
      try networkReachable = Reachability(hostname: "keyman.com")
      try networkReachable?.startNotifier()
    } catch {
      log.error("error thrown starting Reachability notifier:  \(error)")
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

    // Yes, .php.html.  That's how `wget` is set to retrieve it, since Safari won't recognize the contents
    // without the .html ending, it seems.
    let filePath = offlineHelpBundle.path(forResource: "installing-system-keyboard",
                                          ofType: "html",
                                          inDirectory: "start")
    webView.loadRequest(URLRequest(url: URL.init(fileURLWithPath: filePath!)))
  }

  private func loadFromServer() {
    let appVersion = Version.current.majorMinor
    let url = "https://help.keyman.com/products/iphone-and-ipad/\(appVersion.plainString)"
      + "/start/installing-system-keyboard"
    webView.loadRequest(URLRequest(url: URL(string: url)!))
    log.debug("Set up page URL: \(url)")
  }
}
