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

  public var installedPackages: [KeymanPackage] {
    let userResources = Storage.active.userDefaults.userResources ?? []

    var backingPackages: [KeymanPackage] = []
    for resource in userResources {
      if let package = KeymanPackage.parse(Storage.active.resourceDir(for: resource)!) {
        // On successful parse, just ensure that we haven't already listed the package.
        if !backingPackages.contains(where: { $0.id == package.id }) {
          backingPackages.append(package)
        }
      }
    }

    return backingPackages
  }

  public func installState(forPackage key: KeymanPackage.Key) -> KeymanPackage.InstallationState {
    return installState(forPackage: key, withManager: ResourceDownloadManager.shared)
  }

  // For mocked test use.
  internal func installState(forPackage key: KeymanPackage.Key, withManager downloadManager: ResourceDownloadManager) -> KeymanPackage.InstallationState {
    let localCachePath = self.cachedPackagePath(forKey: key)

    if let package = getInstalledPackage(withKey: key) {
      return package.installState
    } else if downloadManager.downloader.containsPackageKeyInQueue(matchingKey: key) {
      return .downloading
    } else if FileManager.default.fileExists(atPath: localCachePath.path) {
      return .pending
    } else {
      return .none
    }
  }

  public func getInstalledPackage<Resource: LanguageResource>(for resource: Resource) -> Resource.Package? {
    if let packageDir = Storage.active.resourceDir(for: resource) {
      return KeymanPackage.parse(packageDir) as? Resource.Package
    } else {
      return nil
    }
  }

  public func getInstalledPackage(withKey key: KeymanPackage.Key) -> KeymanPackage? {
    return KeymanPackage.parse(Storage.active.packageDir(forKey: key))
  }

  internal func packageDownloadTempPath(forKey key: KeymanPackage.Key) -> URL {
    let documentDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    let tempFilename = KeymanPackage.baseFilename(for: key)
    let url = documentDir.appendingPathComponent("\(tempFilename).partial") // marks it as a download in progress
    return url
  }

  internal func cachedPackagePath(forKey key: KeymanPackage.Key) -> URL {
    let documentDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    let filename = KeymanPackage.baseFilename(for: key)
    let url = documentDir.appendingPathComponent(filename)
    return url
  }

  /**
   * Apple doesn't provide a method that performs copy-and-overwrite functionality.  This function fills in that gap.
   */
  internal func copyWithOverwrite(from source: URL, to destination: URL) throws {
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

  internal func typedPromptForPackageInstall<Resource: LanguageResource, Package: TypedKeymanPackage<Resource>>(
                                   of package: Package,
                                   in rootVC: UIViewController,
                                   isCustom: Bool,
                                   successHandler: ((Package) -> Void)? = nil) -> PackageInstallViewController<Resource>
  where Package == Resource.Package {
    return PackageInstallViewController(for: package, completionHandler: { fullIDs in

      if let fullIDs = fullIDs {
        do {
          try ResourceFileManager.shared.install(resourcesWithIDs: fullIDs, from: package)

          if package is KeyboardKeymanPackage {
            // TODO(?): We might should attempt an automatic download + install of lexical models
            // for the specified language codes.
          }
        } catch {
          if let kmpError = error as? KMPError {
            let alert = self.buildKMPError(kmpError)
            rootVC.present(alert, animated: true, completion: nil)
            return
          }
        }

        let alert = self.buildSimpleAlert(title: NSLocalizedString("success-title", bundle: engineBundle, comment: ""),
                                          message: NSLocalizedString("success-install", bundle: engineBundle, comment: ""),
                                          completionHandler: { successHandler?(package) })
        rootVC.present(alert, animated: true, completion: nil)
      }
    })
  }

  public func promptPackageInstall(of package: KeymanPackage,
                                   in rootVC: UIViewController,
                                   isCustom: Bool,
                                   successHandler: ((KeymanPackage) -> Void)? = nil) {
    var vc: UIViewController? = nil
    if let kbdPackage = package as? KeyboardKeymanPackage {
      vc = typedPromptForPackageInstall(of: kbdPackage, in: rootVC, isCustom: isCustom) { package in
        successHandler?(package)
      }
    } else if let lmPackage = package as? LexicalModelKeymanPackage {
      vc = typedPromptForPackageInstall(of: lmPackage, in: rootVC, isCustom: isCustom) { package in
        successHandler?(package)
      }
    }

    if let vc = vc {
      let nvc = UINavigationController.init(rootViewController: vc)
      rootVC.present(nvc, animated: true, completion: nil)
    }
  }

  public func buildKMPError(_ error: KMPError) -> UIAlertController {
    return buildSimpleAlert(title: "Error", message: error.localizedDescription)
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

  /**
   * Searches the specified package for a language resource with the indicated resource-language-code "full ID" key,
   * importing the package's files and installing the indicated resource-language pairing upon success.
   *
   * The`resourcesWithIDs:` variant is better optimized for installing multiple resources from the same package.
   */
  public func install<FullID: LanguageResourceFullID> (
                        resourceWithID fullID: FullID,
                        from package: FullID.Resource.Package) throws
                        where FullID.Resource.Package: TypedKeymanPackage<FullID.Resource> {
    try install(resourcesWithIDs: [fullID], from: package)
  }

  /**
   * Searches the specified package for language resources with the indicated resource-language-code "full ID" keys
   * importing the package's files and installing the indicated resource-language pairings upon success.
   */
  public func install<FullID: LanguageResourceFullID> (
                        resourcesWithIDs fullIDs: [FullID], from package: FullID.Resource.Package) throws
                        where FullID.Resource.Package: TypedKeymanPackage<FullID.Resource> {
    if fullIDs.contains(where: { package.findResource(withID: $0) == nil }) {
      let missingResource = fullIDs.first(where: { package.findResource(withID: $0) == nil })!
      log.error("Resource with full ID \(missingResource.description) not in package")
      throw KMPError.resourceNotInPackage
    }

    do {
      try copyWithOverwrite(from: package.sourceFolder,
                            to: Storage.active.packageDir(for: package)!)
    } catch {
      log.error("Could not create installation directory and/or copy resources: \(error)")
      throw KMPError.fileSystem
    }

    let updatables: [FullID] = findPotentialUpdates(in: package, ignoring: [] as [FullID.Resource]).map { return $0.typedFullID }
    let fullList = fullIDs + updatables

    fullList.forEach { addResource(package.findResource(withID: $0)!) }
  }

  internal func findPotentialUpdates<Resource: LanguageResource,
                                     Package: TypedKeymanPackage<Resource>> (
                                       in package: Package,
                                       ignoring resourcesToIgnore: [Resource] = []) -> [Resource] {
    let installedResources = Storage.active.userDefaults.userResources(ofType: Resource.self) ?? []
    var updatableResources: [Resource] = []

    installedResources.forEach { resource in
      // If there's no package ID, default to the resource's ID.
      // If the package ID matches the resource's package ID and we're not ignoring the resource,
      // check to ensure that the package does contain the resource.
      if (resource.packageID ?? resource.id) == package.id,
         !resourcesToIgnore.contains(where: { $0.typedFullID == resource.typedFullID }) {
        if let updatable = package.findResource(withID: resource.typedFullID) {
          updatableResources.append(updatable)
        }
      }
    }

    return updatableResources
  }

  internal func addResource<Resource: LanguageResource>(_ resource: Resource) {
    let path = Storage.active.resourceURL(for: resource)!.path
    if !FileManager.default.fileExists(atPath: path) {
      log.error("Could not add resource of type: \(resource.fullID.type) with ID: \(resource.id) because the resource file does not exist")
      return
    }

    // Get keyboards list if it exists in user defaults, otherwise create a new one
    let userDefaults = Storage.active.userDefaults

    // Local, inline func used by the block following it.
    func addOrAppend(_ resource: Resource, to resourceList: [Resource]) -> [Resource] {
      var list = resourceList
      // Update resource if it exists
      if let index = resourceList.firstIndex(where: { $0.typedFullID == resource.typedFullID }) {
        list[index] = resource
      } else {
        list.append(resource)
      }
      return list
    }

    // Use the func we just declared while performing proper Swift type coersion.
    if resource is InstallableKeyboard {
      let resourceList = addOrAppend(resource, to: userDefaults.userKeyboards as? [Resource] ?? [])
      userDefaults.userKeyboards = (resourceList as! [InstallableKeyboard])
    } else if resource is InstallableLexicalModel {
      let resourceList = addOrAppend(resource, to: userDefaults.userLexicalModels as? [Resource] ?? [])
      userDefaults.userLexicalModels = (resourceList as! [InstallableLexicalModel])
    } else {
      fatalError("Cannot install instance of unexpected LanguageResource subclass")
    }

    userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
    userDefaults.synchronize()
    log.info("Added \(resource.fullID.type) with ID: \(resource.id) and language code: \(resource.languageID)")
  }
}
