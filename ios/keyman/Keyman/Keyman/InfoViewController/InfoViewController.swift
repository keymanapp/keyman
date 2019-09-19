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

  private var networkReachable: Reachability?

  convenience init() {
    if UIDevice.current.userInterfaceIdiom == .phone {
      self.init(nibName: "InfoViewController_iPhone", bundle: nil)
    } else {
      self.init(nibName: "InfoViewController_iPad", bundle: nil)
    }
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    extendedLayoutIncludesOpaqueBars = true
    webView?.delegate = self
    NotificationCenter.default.addObserver(self, selector: #selector(self.networkStatusChanged),
        name: NSNotification.Name.reachabilityChanged, object: nil)
    networkReachable = Reachability(hostname: "www.keyman.com")
    do {
      try networkReachable?.startNotifier()
    } catch {
      log.error("error starting Reachability notifier: \(error)")
    }
  }

  @objc func networkStatusChanged(_ notification: Notification) {
    reloadKeymanHelp()
  }

  func reloadKeymanHelp() {
    let networkStatus = networkReachable?.connection
    switch networkStatus {
    case Reachability.Connection.none?:
      loadFromLocal()
    default:
      loadFromServer()
    }
  }

  private func loadFromLocal() {
    let offlineHelpBundle = Bundle(path: Bundle.main.path(forResource: "OfflineHelp", ofType: "bundle")!)!

    // Yes, .php.html.  That's how `wget` is set to retrieve it, since Safari won't recognize the contents
    // without the .html ending, it seems.
    let filePath = offlineHelpBundle.path(forResource: "index.php", ofType: "html", inDirectory: nil)
    webView.loadRequest(URLRequest(url: URL.init(fileURLWithPath: filePath!)))
  }

  private func loadFromServer() {
    let appVersion = Version.current
    let url = "https://help.keyman.com/products/iphone-and-ipad/\(appVersion.string)/?embed=ios"
    webView.loadRequest(URLRequest(url: URL(string: url)!))
    log.debug("Info page URL: \(url)")
  }
}
