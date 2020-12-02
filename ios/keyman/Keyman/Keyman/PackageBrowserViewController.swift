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
class PackageBrowserViewController: UIDocumentBrowserViewController, UIDocumentBrowserViewControllerDelegate {

    override func viewDidLoad() {
        super.viewDidLoad()

        delegate = self

        allowsDocumentCreation = false
        allowsPickingMultipleItems = false

        // Update the style of the UIDocumentBrowserViewController
        // browserUserInterfaceStyle = .dark
        // view.tintColor = .white

        // Specify the allowed content types of your application via the Info.plist.

        // Do any additional setup after loading the view.
    }

    // MARK: UIDocumentBrowserViewControllerDelegate

    func documentBrowser(_ controller: UIDocumentBrowserViewController, didPickDocumentsAt documentURLs: [URL]) {
        guard let sourceURL = documentURLs.first else { return }

        // Present the Document View Controller for the first document that was picked.
        // If you support picking multiple items, make sure you handle them all.
        doInstall(of: sourceURL)
    }

    func documentBrowser(_ controller: UIDocumentBrowserViewController,
                         didImportDocumentAt sourceURL: URL,
                         toDestinationURL destinationURL: URL) {
        // Present the Document View Controller for the new newly created document
        doInstall(of: destinationURL)
    }

    func documentBrowser(_ controller: UIDocumentBrowserViewController,
                         failedToImportDocumentAt documentURL: URL,
                         error: Error?) {
        // Make sure to handle the failed import appropriately, e.g., by presenting an error message to the user.
    }

    // MARK: Document Presentation

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
          // Report completion!
          activitySpinner.stopAnimating()
          activitySpinner.removeFromSuperview()
          self.view.isUserInteractionEnabled = true
          self.dismiss(animated: true, completion: nil)
        }
      }
      packageInstaller.promptForLanguages(inNavigationVC: self.navigationController!)
    }
}
