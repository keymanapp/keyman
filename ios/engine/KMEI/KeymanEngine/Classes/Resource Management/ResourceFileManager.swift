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
  public func prepareKMPInstall(from url: URL) throws -> KeymanPackage {
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
      throw KMPError.copyFiles
    }

    var extractionFolder = cacheDirectory
    extractionFolder.appendPathComponent("temp/\(archiveUrl.lastPathComponent)")

    if let kmp = try KeymanPackage.extract(fileUrl: archiveUrl, destination: extractionFolder) {
      return kmp
    } else {
      throw KMPError.invalidPackage
    }
  }

  /**
   * Use this function to "install" external KMP files to within the Keyman app's alloted iOS file management domain.
   * Note that we don't request permissions to support opening/modifying files "in place," so  .kmps should already be
   * located in app-space (by use of `importFile`)  before unzipping them.
   */
  @available(*, deprecated)
  public func prepareKMPInstall(from url: URL, completionHandler: @escaping (KeymanPackage?, Error?) -> Void) {
    do {
      let kmp = try prepareKMPInstall(from: url)
      completionHandler(kmp, nil)
    } catch {
      log.error(error)
      completionHandler(nil, error)
    }
  }

  /**
   * A  utility version of `prepareKMPInstall` that displays default UI alerts if errors occur when preparing a KMP for installation.
   *
   * The completion handler will only be called when a package is successfully "prepared".
   */
  @available(*, deprecated)
  public func prepareKMPInstall(from url: URL, alertHost: UIViewController, completionHandler: @escaping (KeymanPackage) -> Void) {
    if let package = prepareKMPInstall(from: url, alertHost: alertHost) {
      completionHandler(package)
    }
  }

  /**
   * A  utility version of `prepareKMPInstall` that displays default UI alerts if errors occur when preparing a KMP for installation.
   */
  public func prepareKMPInstall(from url: URL, alertHost: UIViewController) -> KeymanPackage? {
    do {
      return try self.prepareKMPInstall(from: url)
    } catch {
      let alert: UIAlertController
      if let kmpError = error as? KMPError {
        alert = self.buildKMPError(kmpError)
      } else {
        alert = self.buildKMPError(KMPError.copyFiles)
      }

      alertHost.present(alert, animated: true, completion: nil)
      return nil
    }
  }

  /**
   * Similar to `preparePackageInstall`, but the resuting `KeymanPackage` cannot be used for installation.  Use when you
   * want information about a package's contents when not immediately looking to install its resources.
   */
  public func getPackageInfo(for url: URL) -> KeymanPackage? {
    // Facilitates clean retrieval of a package's metadata by temporarily extracting
    // its contents just long enough to parse the kmp.json.
    do {
      let package = try self.prepareKMPInstall(from: url)
      try FileManager.default.removeItem(at: package.sourceFolder)
      return package
    } catch {
      log.error("Error occurred attempting to extract metadata for KMP at \(String(describing: url)): \(String(describing: error))")
      return nil
    }
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
  public func finalizePackageInstall(_ package: KeymanPackage, isCustom: Bool) throws {
    // Time to pass the package off to the final installers - the parse__KMP methods.
    // TODO: (14.0+) These functions should probably be refactored to within this class eventually.
    if package.isKeyboard() {
      try Manager.shared.parseKbdKMP(package.sourceFolder, isCustom: isCustom)
    } else {
      try Manager.parseLMKMP(package.sourceFolder, isCustom: isCustom)
    }

    // Note:  package.sourceFolder is a temporary directory, as set by preparePackageInstall.
    try FileManager.default.removeItem(at: package.sourceFolder)
  }

  @available(*, deprecated)
  public func finalizePackageInstall(_ package: KeymanPackage, isCustom: Bool, completionHandler: (Error?) -> Void) {
    do {
      try finalizePackageInstall(package, isCustom: isCustom)
      completionHandler(nil)
    } catch {
      log.error(error)
      completionHandler(error)
    }
  }

  public func install<ResourceType: LanguageResource,
                      PackageType: TypedKeymanPackage<ResourceType>> (
                        resourceWithID fullID: ResourceType.FullID,
                        from package: PackageType) throws {

    guard let resource = package.findResource(withID: fullID) else {
      throw KMPError.resourceNotInPackage
    }

    // (source, destination)
    var installableFiles: [(String, URL)] = [(resource.sourceFilename, Storage.active.resourceURL(for: resource)!)]
    let installableFonts: [(String, URL)] = resource.fonts.map { font in
      // KMPs only list a single font file for each entry whenever one is included.
      // A pre-existing assumption.
      let fontFile = font.source[0]
      return (fontFile, Storage.active.fontURL(forResource: resource, filename: fontFile)!)
    }

    installableFiles.append(contentsOf: installableFonts)

    do {
      try FileManager.default.createDirectory(at: Storage.active.resourceDir(for: resource)!,
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create installation directory: \(error)")
      throw KMPError.fileSystem
    }

    do {
      for item in installableFiles {
        var filePath = package.sourceFolder
        filePath.appendPathComponent(item.0)
        try copyWithOverwrite(from: filePath, to: item.1)
      }
    } catch {
      log.error("Error installing the resource: \(error)")
      throw KMPError.copyFiles
    }

    // There's no generalized method for this quite yet.  Manager doesn't need even
    // more of these.
    if let keyboard = resource as? InstallableKeyboard {
      Manager.shared.addKeyboard(keyboard)
    } else if let lexicalModel = resource as? InstallableLexicalModel {
      Manager.addLexicalModel(lexicalModel)
    } else {
      fatalError("Cannot install instance of unexpected LanguageResource subclass")
    }
  }
}
