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
    if source.startAccessingSecurityScopedResource() {
      defer { source.stopAccessingSecurityScopedResource() }
      try fileManager.copyItem(at: source, to: destination)
    } else {
      try fileManager.copyItem(at: source, to: destination)
    }
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

    // Since it's possible to request an install from a KMP in our owned document space,
    // we need to check that it's not already in place where we want it.
    if url == destinationUrl {
      return url
    }

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
   * Note that we don't request permissions to support opening/modifying files "in place," so  .kmps should already be
   * located in app-space (by use of `importFile`)  before unzipping them.
   */
  public func prepareKMPInstall(from url: URL, completionHandler: @escaping (KeymanPackage?, Error?) -> Void) {
    // Once selected, start the standard install process.
    log.info("Installing KMP from \(url)")

    // Step 1: Copy it to a temporary location, making it a .zip in the process
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    var archiveUrl = cacheDirectory
    archiveUrl.appendPathComponent("\(url.lastPathComponent).zip")

    do {
      try copyWithOverwrite(from: url, to: archiveUrl)
    } catch {
      log.error(error)
      completionHandler(nil, KMPError.copyFiles)
      return
    }

    var extractionFolder = cacheDirectory
    extractionFolder.appendPathComponent("temp/\(archiveUrl.lastPathComponent)")

    KeymanPackage.extract(fileUrl: archiveUrl, destination: extractionFolder, complete: { kmp in
      if let kmp = kmp {
        completionHandler(kmp, nil)
      } else {
        log.error(KMPError.invalidPackage)
        completionHandler(nil, KMPError.invalidPackage)
      }
    })
  }

  /**
   * A  utility version of `prepareKMPInstall` that displays default UI alerts if errors occur when preparing a KMP for installation.
   */
  public func prepareKMPInstall(from url: URL, alertHost: UIViewController, completionHandler: @escaping (KeymanPackage) -> Void) {
    self.prepareKMPInstall(from: url, completionHandler: { package, error in
      if error != nil {
        let alert = self.buildKMPError(KMPError.copyFiles)
        alertHost.present(alert, animated: true, completion: nil)
      } else {
        completionHandler(package!)
      }
    })
  }

  public func promptPackageInstall(of package: KeymanPackage,
                                   in rootVC: UIViewController,
                                   isCustom: Bool,
                                   successHandler: ((KeymanPackage) -> Void)? = nil) {
    let vc = PackageInstallViewController(for: package, isCustom: isCustom, completionHandler: { error in
      if let err = error {
        if let kmpError = err as? KMPError {
          let alert = self.buildKMPError(kmpError)
          rootVC.present(alert, animated: true, completion: nil)
        }
      } else {
        let alert = self.buildSimpleAlert(title: "Success", message: "Installed successfully.", completionHandler: {
            successHandler?(package)
          })
        rootVC.present(alert, animated: true, completion: nil)
      }
    })

    let nvc = UINavigationController.init(rootViewController: vc)
    rootVC.present(nvc, animated: true, completion: nil)
  }

  public func buildKMPError(_ error: KMPError) -> UIAlertController {
    return buildSimpleAlert(title: "Error", message: error.rawValue)
  }

  public func buildSimpleAlert(title: String, message: String, completionHandler: (() -> Void)? = nil ) -> UIAlertController {
    let alertController = UIAlertController(title: title, message: message,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertAction.Style.default,
                                            handler: { _ in
                                              completionHandler?()
                                            }))

    //UIApplication.shared.keyWindow?.rootViewController?.present(alertController, animated: true, completion: nil)
    return alertController
  }

  /**
   * Performs the actual installation of a package's resources once confirmation has been received from the user.
   */
  public func finalizePackageInstall(_ package: KeymanPackage, isCustom: Bool, completionHandler: (Error?) -> Void) {
    do {
      // Time to pass the package off to the final installers - the parse__KMP methods.
      // TODO: (14.0+) These functions should probably be refactored to within this class eventually.
      if package.isKeyboard() {
        try Manager.shared.parseKbdKMP(package.sourceFolder, isCustom: isCustom)
      } else {
        try Manager.parseLMKMP(package.sourceFolder, isCustom: isCustom)
      }
      completionHandler(nil)
    } catch {
      log.error(error as! KMPError)
      completionHandler(error)
    }

    //this can fail gracefully and not show errors to users
    do {
      try FileManager.default.removeItem(at: package.sourceFolder)
    } catch {
      log.error("unable to delete temp files: \(error)")
      completionHandler(error)
    }
  }
}
