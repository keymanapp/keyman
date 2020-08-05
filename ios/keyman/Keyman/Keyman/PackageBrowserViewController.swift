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
        if let kbdPackage = package as? KeyboardKeymanPackage {
          let packageInstaller = AssociatingPackageInstaller(for: kbdPackage,
                                                             withAssociators: [.lexicalModels])
          packageInstaller.promptForLanguages(inNavigationVC: self.navigationController!)
        } else {
          // We choose to prompt the user for comfirmation, rather
          // than automatically installing the package.
          rfm.promptPackageInstall(of: package, in: self, isCustom: true, successHandler: { _ in
            // Auto-dismiss the document browser upon successful KMP install.
            // It's likely quite rare that someone would want to install 2+ at once.
            self.navigationController?.popViewController(animated: true)
          })
        }
      }
    }
}
