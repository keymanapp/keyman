//
//  PackageBrowserViewController.swift
//  Keyman
//
//  Created by Joshua Horton on 12/6/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import UIKit
import SwiftUI
import KeymanEngine

@available(iOS 11.0, *)
class PackageBrowserViewController: UIDocumentPickerViewController, UIDocumentPickerDelegate {
  private weak var navVC: UINavigationController?

  init(documentTypes allowedUTIs: [String], in mode: UIDocumentPickerMode, navVC: UINavigationController) {
    super.init(documentTypes: allowedUTIs, in: mode)

    self.navVC = navVC
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func viewDidLoad() {
      super.viewDidLoad()
      delegate = self

      allowsMultipleSelection = false
      self.title = NSLocalizedString("menu-settings-install-from-file",
                                     bundle: Bundle(for: Manager.self),
                                     comment: "")

      if #available(iOS 13.0, *) {
        // Easily dismissable without the extra button.
      } else {
        self.navigationItem.setLeftBarButton(
          UIBarButtonItem(barButtonSystemItem: .cancel, target: self, action: #selector(self.doCancel)),
          animated: true)
      }
      self.modalPresentationStyle = .fullScreen
  }

  @objc func doCancel() {
    self.dismiss(animated: true, completion: nil)
  }

  // MARK: UIDocumentPickerDelegate

  func documentPicker(_ controller: UIDocumentPickerViewController, didPickDocumentsAt documentURLs: [URL]) {
      guard let sourceURL = documentURLs.first else { return }

      doInstall(of: sourceURL)
  }

  // MARK: Package processing

  func doInstall(of url: URL) {
    // Once selected, start the standard install process.
    let rfm = ResourceFileManager.shared

    guard let destinationUrl = rfm.importFile(url) else {
      return
    }

    if let package = rfm.prepareKMPInstall(from: destinationUrl, alertHost: self) {
      // These type checks are necessary due to generic constraints.
      if let kbdPackage = package as? KeyboardKeymanPackage {
        doPrompt(for: kbdPackage, withAssociators: [.lexicalModels])
      } else if let lmPackage = package as? LexicalModelKeymanPackage {
        doPrompt(for: lmPackage)
      }
    }
  }

  private func doPrompt<Resource: LanguageResource, Package: TypedKeymanPackage<Resource>>(
      for package: Package,
      withAssociators associators: [AssociatingPackageInstaller<Resource, Package>.Associator] = [])
  where Resource.Package == Package {
    let activitySpinner = Alerts.constructActivitySpinner()
    activitySpinner.center = view.center

    let packageInstaller = AssociatingPackageInstaller(for: package,
                                                       withAssociators: associators) { status in
      if status == .starting {
        // Start a spinner!
        activitySpinner.startAnimating()
        self.view.addSubview(activitySpinner)

        activitySpinner.centerXAnchor.constraint(equalTo: self.view.centerXAnchor).isActive = true
        activitySpinner.centerYAnchor.constraint(equalTo: self.view.centerYAnchor).isActive = true
        self.view.isUserInteractionEnabled = false
      } else if status == .complete {
        // Report completion!   Only reached if installation actually happens.
        activitySpinner.stopAnimating()
        activitySpinner.removeFromSuperview()
        self.view.isUserInteractionEnabled = true

        // Do not animate the dismissal:  the welcome-page's dismissal
        // already provides enough animation.
        if let navVC = self.navVC {
          navVC.dismiss(animated: false, completion: nil)
        }
      }
    }

    if let navVC = self.navVC {
      packageInstaller.promptForLanguages(inNavigationVC: navVC)
    }
  }
}
