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
    networkReachable = Reachability(hostname: "www.keyman.com")
    do {
      try networkReachable?.startNotifier()
    } catch {
      log.error("error thrown starting Reachability notifier:  \(error)")
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

  @objc func dismissView() {
    dismiss(animated: true, completion: nil)
  }

  private func loadFromLocal() {
    let filePath = Bundle.main.path(forResource: "setup", ofType: "html", inDirectory: nil)
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
    let url = "http://keyman.com/iphone-and-ipad/app/systemkeyboard/index.php" +
        "?active=\(currentKeyboardId)&installed=\(installedKeyboards)"
    webView.loadRequest(URLRequest(url: URL(string: url)!))
    log.debug("Set up page URL: \(url)")
  }

  func webView(_ webView: UIWebView, shouldStartLoadWith request: URLRequest,
               navigationType: UIWebViewNavigationType) -> Bool {
    if navigationType == UIWebViewNavigationType.linkClicked {
      if let url = request.url {
        UIApplication.shared.openURL(url)
      }
      return false
    }

    return true
  }
}
