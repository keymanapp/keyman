//
//  PackageInstallViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 12/18/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import WebKit

public class PackageInstallViewController<Resource: LanguageResource>: UIViewController {
  public typealias CompletionHandler = ([Resource.FullID]?) -> Void

  let package: Resource.Package
  var wkWebView: WKWebView?
  let completionHandler: CompletionHandler
  let isCustom: Bool

  public init(for package: Resource.Package, isCustom: Bool, completionHandler: @escaping CompletionHandler) {
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

    let cancelBtn = UIBarButtonItem(title: NSLocalizedString("command-cancel", bundle: engineBundle, comment: ""), style: .plain,
                                    target: self,
                                    action: #selector(cancelBtnHandler))
    let installBtn = UIBarButtonItem(title: NSLocalizedString("command-install", bundle: engineBundle, comment: ""), style: .plain,
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
    dismiss(animated: true, completion: {
      self.completionHandler(nil)
    })
  }

  @objc func installBtnHandler() {
    dismiss(animated: true, completion: {
      // A stop-gap pending further changes.
      let defaultResource = self.package.installableResourceSets[0] as! [Resource]
      switch self.package.resourceType() {
        case .keyboard:
          self.completionHandler([defaultResource[0].typedFullID])
        case .lexicalModel:
          self.completionHandler(defaultResource.map { $0.typedFullID })
      }
    })
  }
}
