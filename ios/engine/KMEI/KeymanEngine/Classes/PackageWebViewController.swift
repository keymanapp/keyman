//
//  PackageWebViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 23/9/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit
import WebKit

class PackageWebViewController: UIViewController, WKNavigationDelegate {
  private var webView: WKWebView!
  private let package: KeymanPackage
  private let homePage: KeymanPackagePage

  private let onDismissal: (() -> Void)?

  public init?(for package: KeymanPackage, page: KeymanPackagePage, onDismissal: (() -> Void)? = nil) {
    self.package = package
    self.homePage = page
    self.onDismissal = onDismissal

    switch(page) {
      case .readme:
        // We always provide a default readme text for packages, even if it's raw text.
        break
      default:
        if package.pageURL(for: page) == nil {
          return nil
        }
    }

    super.init(nibName: nil, bundle: nil)

    _ = view
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func loadView() {
    let config = WKWebViewConfiguration()
    let prefs = WKPreferences()
    prefs.javaScriptEnabled = true
    config.preferences = prefs

    webView = WKWebView(frame: CGRect.zero, configuration: config)
    webView!.isOpaque = false
    webView!.backgroundColor = UIColor.white
    webView!.navigationDelegate = self
    webView!.scrollView.isScrollEnabled = true

    view = webView
    reloadHomePage()
  }

  private func reloadHomePage() {
    switch(self.homePage) {
      case .readme: // Cannot if-check against _not_ matching an enum in an if.  Thanks, Swift.
        if package.pageURL(for: .readme) == nil {
          webView.loadHTMLString(package.infoHtml(), baseURL: package.sourceFolder)
        } else {
          fallthrough
        }
      default:
        let url = package.pageURL(for: self.homePage)! // we already know it exists
        webView.loadFileURL(url, allowingReadAccessTo: package.sourceFolder)
    }
  }

  @objc func onClose() {
    self.onDismissal?()
  }
}
