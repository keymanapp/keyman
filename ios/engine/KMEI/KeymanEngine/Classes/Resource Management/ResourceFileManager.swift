//
//  ResourceFileManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on December 18, 2019.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

/**
 * This class stores common methods used for installing language resources, regardless of source.
 *
 * It also contains methods for general-purpose installation of language resources from .kmp files.
 */
public class ResourceFileManager {
  public static let shared = ResourceFileManager()

  fileprivate init() {
  }

  /**
   * Apple doesn't provide a method that performs copy-and-overwrite functionality.  This function fills in that gap.
   */
  private func copyWithOverwrite(from source: URL, to destination: URL) throws {
    let fileManager = FileManager.default

    // For now, we'll always allow overwriting.
    if fileManager.fileExists(atPath: destination.path) {
      try fileManager.removeItem(at: destination)
    }

    // Throws an error if the destination file already exists, and there's no
    // built-in override parameter.  Hence, the previous if-block.
    try fileManager.copyItem(at: source, to: destination)
  }

  /**
   * Use this function to "import" a file from outside the app's designated file system area to a new location within,
   * copying the original.  It will be placed within the app's Documents folder.
   *
   * Returns the app-owned destination path, usable for subsequent file operations.
   */
  public func importFile(_ url: URL) -> URL? {
    var destinationUrl = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    destinationUrl.appendPathComponent(url.lastPathComponent)

    do {
      try copyWithOverwrite(from: url, to: destinationUrl)
      return destinationUrl
    } catch {
      log.error(error)
      return nil
    }
  }

  /**
   * Use this function to "install" external KMP files to within the Keyman app's alloted iOS file management domain.
   * Note that we don't request permissions to support opening/modifying files "in place," so we need to copy .kmps
   * before unzipping them.
   *
   * This implementation does not change how files are managed by the app; only where the file management code
   * is located.
   */
  @available(iOSApplicationExtension, unavailable)
  public func installFile(_ url: URL) {
      // Once selected, start the standard install process.
      log.info("Installing KMP from \(url)")

      // Step 1: Copy it to a temporary location, making it a .zip in the process
      var destinationUrl = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
      destinationUrl.appendPathComponent("\(url.lastPathComponent).zip")

      do {
        try copyWithOverwrite(from: url, to: destinationUrl)
        installAdhocKeyboard(url: destinationUrl)
      } catch {
        showKMPError(KMPError.copyFiles)
        log.error(error)
      }
  }

  @available(iOSApplicationExtension, unavailable)
  private func installAdhocKeyboard(url: URL) {
    let documentsDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    var destination = documentsDirectory
    destination.appendPathComponent("temp/\(url.lastPathComponent)")

    KeymanPackage.extract(fileUrl: url, destination: destination, complete: { kmp in
      if let kmp = kmp {
        self.promptAdHocInstall(kmp)
      } else {
        self.showKMPError(KMPError.invalidPackage)
      }
    })
  }

  @available(iOSApplicationExtension, unavailable)
  private func promptAdHocInstall(_ kmp: KeymanPackage) {
    let vc = PackageInstallViewController(for: kmp, completionHandler: { error in
      if let err = error {
        if let kmpError = err as? KMPError {
          self.showKMPError(kmpError)
        }
      } else {
        self.showSimpleAlert(title: "Success", message: "Installed successfully.")
      }
    })

    let nvc = UINavigationController.init(rootViewController: vc)
    UIApplication.shared.keyWindow?.rootViewController?.present(nvc, animated: true, completion: nil)
  }

  @available(iOSApplicationExtension, unavailable)
  public func showKMPError(_ error: KMPError) {
    showSimpleAlert(title: "Error", message: error.rawValue)
  }

  @available(iOSApplicationExtension, unavailable)
  public func showSimpleAlert(title: String, message: String) {
    let alertController = UIAlertController(title: title, message: message,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertAction.Style.default,
                                            handler: nil))

    UIApplication.shared.keyWindow?.rootViewController?.present(alertController, animated: true, completion: nil)
  }
}
