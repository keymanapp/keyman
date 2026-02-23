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

  public init?(for package: KeymanPackage, page: KeymanPackagePage) {
    self.package = package
    self.homePage = page

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

    /**
      In iPadOS 16 and above WKWebView defaults to lying about its user-agent,
      telling the web server that it is a mac. We can avoid this with .mobile:
      */
    let pref = WKWebpagePreferences.init()
    pref.preferredContentMode = .mobile
    config.defaultWebpagePreferences = pref

    // Inject a meta viewport tag into the head of the file if it doesn't exist
    let metaViewportInjection = """
      if(!document.querySelectorAll('meta[name=viewport]').length) {
        let meta=document.createElement('meta');
        meta.name='viewport';
        meta.content='width=device-width, initial-scale=1';
        document.head.appendChild(meta);
      }
      """

    let injection = WKUserScript(source: metaViewportInjection, injectionTime: .atDocumentStart, forMainFrameOnly: true)
    let controller = WKUserContentController()
    controller.addUserScript(injection)
    config.preferences = prefs
    config.userContentController = controller

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

  func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
    guard let _ = webView.url else {
      return
    }
  }

  // Used to intercept links that should be handled externally.
  public func webView(_ webView: WKWebView,
               decidePolicyFor navigationAction: WKNavigationAction,
               decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
    if let link = navigationAction.request.url {
      if link.path.hasPrefix(self.package.sourceFolder.path) {
        // Links from within the package will show up as 'external' in the condition below!
        // So, we check against package-internal links first.
        decisionHandler(.allow)
        return
      } else if UniversalLinks.isExternalLink(link) {
        decisionHandler(.cancel)

        // Kick this link out to an external Safari process.
        UniversalLinks.externalLinkLauncher?(link)
        return
      }
    }

    decisionHandler(.allow)
  }
}
