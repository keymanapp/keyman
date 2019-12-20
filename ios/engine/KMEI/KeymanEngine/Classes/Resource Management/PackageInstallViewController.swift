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

  public init(for package: KeymanPackage, completionHandler: @escaping CompletionHandler) {
    self.package = package
    self.completionHandler = completionHandler
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
  }

  override public func viewWillAppear(_ animated: Bool) {
    wkWebView?.loadHTMLString(package.infoHtml(), baseURL: nil)
  }

  @objc func cancelBtnHandler() {
    dismiss(animated: true, completion: nil)
  }

  @objc func installBtnHandler() {
    dismiss(animated: true, completion: {
      let package = self.package
      do {
        // Time to pass the package off to the final installers - the parse__KMP methods.
        // ... they should probably be moved to ResourceFileManager eventually.
        if package.isKeyboard() {
          try Manager.shared.parseKbdKMP(package.sourceFolder)
        } else {
          try Manager.parseLMKMP(package.sourceFolder)
        }
        self.completionHandler(nil)
      } catch {
        log.error(error as! KMPError)
        self.completionHandler(error)
      }

      //this can fail gracefully and not show errors to users
      do {
        try FileManager.default.removeItem(at: package.sourceFolder)
      } catch {
        log.error("unable to delete temp files: \(error)")
        self.completionHandler(error)
      }
    })
  }
}
