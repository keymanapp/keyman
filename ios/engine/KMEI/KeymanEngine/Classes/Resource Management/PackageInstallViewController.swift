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

  // Needed to support iOS 9 + 10.
  @IBOutlet weak var webViewContainer: UIView!
  @IBOutlet weak var lblVersion: UILabel!
  @IBOutlet weak var lblAuthor: UILabel!

  let package: Resource.Package
  var wkWebView: WKWebView?
  let completionHandler: CompletionHandler
  let isCustom: Bool

  public init(for package: Resource.Package, isCustom: Bool, completionHandler: @escaping CompletionHandler) {
    self.package = package
    self.completionHandler = completionHandler
    self.isCustom = isCustom

    super.init(nibName: "PackageInstallView", bundle: Bundle.init(for: PackageInstallViewController.self))

    _ = view
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override public func loadView() {
    super.loadView()

    wkWebView = WKWebView.init(frame: webViewContainer.frame)
    wkWebView!.backgroundColor = .white
    wkWebView!.translatesAutoresizingMaskIntoConstraints = false
    webViewContainer.addSubview(wkWebView!)

//    // Ensure the web view fills its available space.
//    wkWebView?.autoresizingMask = [.flexibleWidth, .flexibleHeight]
    wkWebView!.topAnchor.constraint(equalTo: webViewContainer.topAnchor).isActive = true
    wkWebView!.leadingAnchor.constraint(equalTo: webViewContainer.leadingAnchor).isActive = true

    wkWebView!.bottomAnchor.constraint(equalTo: webViewContainer.bottomAnchor).isActive = true
    wkWebView!.trailingAnchor.constraint(equalTo: webViewContainer.trailingAnchor).isActive = true

    let cancelBtn = UIBarButtonItem(title: "Cancel", style: .plain,
                                    target: self,
                                    action: #selector(cancelBtnHandler))
    let installBtn = UIBarButtonItem(title: "Install", style: .plain,
                                     target: self,
                                     action: #selector(installBtnHandler))

    navigationItem.leftBarButtonItem = cancelBtn
    navigationItem.rightBarButtonItem = installBtn
    navigationItem.title = package.name

    lblVersion.text = "Version: \(package.version)"
    if let author = package.metadata.info?.author?.description {
      lblAuthor.text = "Author: \(author)"
    } else {
      lblAuthor.isHidden = true
    }
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
