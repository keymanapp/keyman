//
//  SetUpViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

// TODO: Refactor common functionality from InfoViewController
class SetUpViewController: UIViewController, UIWebViewDelegate {
  @IBOutlet var webView: UIWebView!

  private var networkReachable: Reachability?

  override func viewDidLoad() {
    super.viewDidLoad()

    let navBar = view.viewWithTag(786586) as? UINavigationBar
    let doneButton = navBar?.topItem?.rightBarButtonItem
    doneButton?.target = self
    doneButton?.action = #selector(self.dismissView)
    webView?.delegate = self
    NotificationCenter.default.addObserver(self, selector: #selector(self.networkStatusChanged),
        name: NSNotification.Name.reachabilityChanged, object: nil)
    networkReachable = Reachability(hostName: "www.keyman.com")
    networkReachable?.startNotifier()
  }

  @objc func networkStatusChanged(_ notification: Notification) {
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

  @objc func dismissView() {
    dismiss(animated: true, completion: nil)
  }

  private func loadFromLocal() {
    let filePath = Bundle.main.path(forResource: "setup", ofType: "html", inDirectory: nil)
    webView.loadRequest(URLRequest(url: URL.init(fileURLWithPath: filePath!)))
  }

  private func loadFromServer() {
    let keyboardInfo = Manager.shared.currentKeyboardInfo
    let currentKeyboardId = keyboardInfo?.id ?? Constants.defaultKeyboard.id
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
    #if DEBUG
      NSLog("Set up page URL: %@", url)
    #endif
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
