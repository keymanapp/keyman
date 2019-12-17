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
        presentDocument(at: sourceURL)
    }

    func documentBrowser(_ controller: UIDocumentBrowserViewController,
                         didImportDocumentAt sourceURL: URL,
                         toDestinationURL destinationURL: URL) {
        // Present the Document View Controller for the new newly created document
        presentDocument(at: destinationURL)
    }

    func documentBrowser(_ controller: UIDocumentBrowserViewController,
                         failedToImportDocumentAt documentURL: URL,
                         error: Error?) {
        // Make sure to handle the failed import appropriately, e.g., by presenting an error message to the user.
    }

    // MARK: Document Presentation

    func presentDocument(at documentUrl: URL) {
        // Once selected, start the standard install process.
        log.info("Installing KMP at \(documentUrl)")

        // Step 1: Copy it to within the app's controlled space, making it a .zip in the process
        var destinationUrl = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
        destinationUrl.appendPathComponent("\(documentUrl.lastPathComponent).zip")

        let fileManager = FileManager.default
        do {
          if fileManager.fileExists(atPath: destinationUrl.path) {
            try fileManager.removeItem(at: destinationUrl)
          }
          try fileManager.copyItem(at: documentUrl, to: destinationUrl)
        } catch {
          showKMPError(KMPError.copyFiles)
          log.error(error)
          return
        }

        // Now, install it as if we'd just downloaded it.
        let resourceManager = ResourceDownloadManager.shared
        guard let lexicalModels = resourceManager.installLexicalModelPackage(at: destinationUrl) else {
          log.info("Could not install KMP at \(documentUrl)")
          return
        }

        log.info("Attempt success!")
    }

    // The following are raw copies from AppDelegate.  Not great, but it's a useful
    // start while developing.
    public func showKMPError(_ error: KMPError) {
      showSimpleAlert(title: "Error", message: error.rawValue)
    }

    public func showSimpleAlert(title: String, message: String) {
      let alertController = UIAlertController(title: title, message: message,
                                              preferredStyle: UIAlertController.Style.alert)
      alertController.addAction(UIAlertAction(title: "OK",
                                              style: UIAlertAction.Style.default,
                                              handler: nil))

      // This part is tweaked (in comparison to the AppDelegate version).
      self.navigationController?.present(alertController, animated: true, completion: nil)
    }
}
