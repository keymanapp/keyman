//
//  InfoViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

class InfoViewController: UIViewController, UIWebViewDelegate {
  @IBOutlet var webView: UIWebView!

  private var networkReachable: Reachability?

  override func viewDidLoad() {
    super.viewDidLoad()
    extendedLayoutIncludesOpaqueBars = true
    webView?.delegate = self
    NotificationCenter.default.addObserver(self, selector: #selector(self.networkStatusChanged),
        name: NSNotification.Name.reachabilityChanged, object: nil)
    networkReachable = Reachability(hostName: "www.keyman.com")
    networkReachable?.startNotifier()
  }

  func networkStatusChanged(_ notification: Notification) {
    reloadKeymanHelp()
  }

  func reloadKeymanHelp() {
    let networkStatus = networkReachable?.currentReachabilityStatus()
    switch networkStatus {
    case NotReachable?:
      loadFromLocal()
    default:
      loadFromServer()
    }
  }

  private func loadFromLocal() {
    let filePath = Bundle.main.path(forResource: "info", ofType: "html", inDirectory: nil)
    webView.loadRequest(URLRequest(url: URL.init(fileURLWithPath: filePath!)))
  }

  private func loadFromServer() {
    let keyboardInfo = KMManager.sharedInstance().currentKeyboardInfo() as? [AnyHashable : String]
    let currentKeyboardId = keyboardInfo?[kKeymanKeyboardIdKey] ?? kKeymanDefaultKeyboardID
    let userData = AppDelegate.activeUserDefaults()
    let keyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) as? [NSDictionary]
    let keyboardIds = keyboards?.flatMap { $0.object(forKey: kKeymanKeyboardIdKey) as? String }
    let installedKeyboards: String
    if let ids = keyboardIds, !ids.isEmpty {
      installedKeyboards = Array(Set(ids)).joined(separator: ",")
    } else {
      installedKeyboards = currentKeyboardId
    }
    let url = "http://keyman.com/iphone-and-ipad/app/?active=\(currentKeyboardId)&installed=\(installedKeyboards)"
    webView.loadRequest(URLRequest(url: URL(string: url)!))
    #if DEBUG
      NSLog("Info page URL: %@", url)
    #endif
  }
}
