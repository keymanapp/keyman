//
//  PackageInstallViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 12/18/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import WebKit

public class PackageInstallViewController: UIViewController {
  public typealias CompletionHandler = (Error?) -> Void

  let package: KeymanPackage
  var wkWebView: WKWebView?
  let completionHandler: CompletionHandler
  let isCustom: Bool

  public init(for package: KeymanPackage, isCustom: Bool, completionHandler: @escaping CompletionHandler) {
    self.package = package
    self.completionHandler = completionHandler
    self.isCustom = isCustom
    super.init(nibName: nil, bundle: nil)

    _ = view
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override public func loadView() {
    wkWebView = WKWebView.init(frame: .zero)
    wkWebView!.backgroundColor = .white
    view = wkWebView!

    // Ensure the web view fills its available space.
    wkWebView?.autoresizingMask = [.flexibleWidth, .flexibleHeight]

    let cancelBtn = UIBarButtonItem(title: "Cancel", style: .plain,
                                    target: self,
                                    action: #selector(cancelBtnHandler))
    let installBtn = UIBarButtonItem(title: "Install", style: .plain,
                                     target: self,
                                     action: #selector(installBtnHandler))

    navigationItem.leftBarButtonItem = cancelBtn
    navigationItem.rightBarButtonItem = installBtn
    navigationItem.title = package.name
  }

  override public func viewWillAppear(_ animated: Bool) {
    if let welcomeURL = package.welcomePageURL {
      wkWebView?.loadFileURL(welcomeURL, allowingReadAccessTo: package.sourceFolder)
    } else {
      wkWebView?.loadHTMLString(package.infoHtml(), baseURL: nil)
    }
  }

  @objc func cancelBtnHandler() {
    dismiss(animated: true, completion: nil)
  }

  @objc func installBtnHandler() {
    dismiss(animated: true, completion: {
      do {
        try ResourceFileManager.shared.finalizePackageInstall(self.package, isCustom: self.isCustom)
        self.completionHandler(nil)
      } catch {
        self.completionHandler(error)
      }
    })
  }
}
