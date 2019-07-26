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
    let filePath = Bundle.main.path(forResource: "info", ofType: "html", inDirectory: nil)
    webView.loadRequest(URLRequest(url: URL.init(fileURLWithPath: filePath!)))
  }

  private func loadFromServer() {
    let keyboardInfo = Manager.shared.currentKeyboard
    let currentKeyboardId = keyboardInfo?.id ?? Defaults.keyboard.id
    let userData = AppDelegate.activeUserDefaults()
    let keyboards = userData.userKeyboards
    let keyboardIds = keyboards?.map { $0.id }
    let installedKeyboards: String
    if let ids = keyboardIds, !ids.isEmpty {
      installedKeyboards = Array(Set(ids)).joined(separator: ",")
    } else {
      installedKeyboards = currentKeyboardId
    }
    let url = "http://keyman.com/iphone-and-ipad/app/?active=\(currentKeyboardId)&installed=\(installedKeyboards)"
    webView.loadRequest(URLRequest(url: URL(string: url)!))
    log.debug("Info page URL: \(url)")
  }
}
