//
//  ResourceFileManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on December 18, 2019.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import UIKit
import os.log

/**
 * This class stores common methods used for installing language resources, regardless of source.
 *
 * It also contains methods for general-purpose installation of language resources from .kmp files.
 */
public class ResourceFileManager {
  public static let shared = ResourceFileManager()

  private var haveRunMigrations: Bool = false

  fileprivate init() {
  }

  func runMigrationsIfNeeded() {
    if !haveRunMigrations {
      // Set here in order to prevent recursively calling itself in case
      // Migrations itself needs to use installation methods.
      haveRunMigrations = true

      // We must make sure that all resources are properly migrated before
      // allowing any new resources to be installed.
      Migrations.migrate(storage: Storage.active)
      Migrations.updateResources(storage: Storage.active)

      if Storage.active.userDefaults.userKeyboards?.isEmpty ?? true {
        Storage.active.userDefaults.userKeyboards = [Defaults.keyboard]

        // Ensure the default keyboard is installed in this case.
        do {
          try Storage.active.installDefaultKeyboard(from: Resources.bundle)
        } catch {
          let message = "Failed to copy default keyboard from bundle: \(error)"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
          SentryManager.capture(error, message:message)
        }
      }
      Migrations.engineVersion = Version.latestFeature
    }

    haveRunMigrations = true
  }

  public var installedPackages: [KeymanPackage] {
    let userResources = Storage.active.userDefaults.userResources ?? []

    var backingPackages: [KeymanPackage] = []
    for resource in userResources {
      if let package = try? KeymanPackage.parse(Storage.active.resourceDir(for: resource)!) {
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
      return try? KeymanPackage.parse(packageDir) as? Resource.Package
    } else {
      return nil
    }
  }

  public func getInstalledPackage(withKey key: KeymanPackage.Key) -> KeymanPackage? {
    return try? KeymanPackage.parse(Storage.active.packageDir(forKey: key))
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

    // If we've been provided a security-scoped resource URL,
    // it needs special handling.  This function needs to accept
    // both scoped & non-scoped URLs.
    if source.startAccessingSecurityScopedResource() { // only succeeds if scoped
      // The Swift version of 'finally'.
      defer { source.stopAccessingSecurityScopedResource() }
      try fileManager.copyItem(at: source, to: destination)
    } else {
      // Not scoped?  No problem!
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
      let message = "\(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(error, message:message)
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
    let message = "Opening KMP from \(url)"
    os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
    SentryManager.breadcrumb(message)

    // Step 1: Copy it to a temporary location, making it a .zip in the process
    let cacheDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    var archiveUrl = cacheDirectory
    archiveUrl.appendPathComponent("\(url.lastPathComponent).zip")

    do {
      try copyWithOverwrite(from: url, to: archiveUrl)
    } catch {
      let errorMessage = "\(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
      throw KMPError.copyFiles
    }

    var extractionFolder = cacheDirectory
    extractionFolder.appendPathComponent("temp/\(archiveUrl.lastPathComponent)")

    // first clear extraction folder to avoid creating duplicates
    try KeymanPackage.clearDirectory(destination: extractionFolder)
    
    do {
      if let package = try KeymanPackage.extract(fileUrl: archiveUrl, destination: extractionFolder) {
        return package
      } else {
        throw KMPError.doesNotExist
      }
    } catch {
      throw error
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
      let errorMessage = "\(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
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

  internal func doInstallPrompt<Resource: LanguageResource, Package: TypedKeymanPackage<Resource>>(
        for package: Package,
        defaultLanguageCode: String? = nil,
        in rootVC: UIViewController,
        withAssociators associators: [AssociatingPackageInstaller<Resource, Package>.Associator] = [],
        successHandler: ((KeymanPackage) -> Void)? = nil) where Resource.Package == Package {
    let activitySpinner = Alerts.constructActivitySpinner()
    activitySpinner.center = rootVC.view.center

    let packageInstaller = AssociatingPackageInstaller(for: package,
                                                       defaultLanguageCode: defaultLanguageCode,
                                                       withAssociators: associators) { status in
      if status == .starting {
        // Start a spinner!
        activitySpinner.startAnimating()
        rootVC.view.addSubview(activitySpinner)

        activitySpinner.centerXAnchor.constraint(equalTo: rootVC.view.centerXAnchor).isActive = true
        activitySpinner.centerYAnchor.constraint(equalTo: rootVC.view.centerYAnchor).isActive = true
        rootVC.view.isUserInteractionEnabled = false
      } else if status == .complete || status == .cancelled {
        // Report completion!
        activitySpinner.stopAnimating()
        activitySpinner.removeFromSuperview()
        rootVC.view.isUserInteractionEnabled = true
        rootVC.dismiss(animated: true) {
          Manager.shared.showKeyboard()
        }
        successHandler?(package)
      }
    }

    if let navVC = rootVC as? UINavigationController {
      packageInstaller.promptForLanguages(inNavigationVC: navVC)
    } else {
      let nvc = UINavigationController.init()
      packageInstaller.promptForLanguages(inNavigationVC: nvc)
      rootVC.present(nvc, animated: true, completion: nil)
    }
  }

  public func promptPackageInstall(of package: KeymanPackage,
                                   in rootVC: UIViewController,
                                   isCustom: Bool,
                                   successHandler: ((KeymanPackage) -> Void)? = nil) {
    if let kbdPackage = package as? KeyboardKeymanPackage {
      doInstallPrompt(for: kbdPackage, in: rootVC, withAssociators: [.lexicalModels], successHandler: successHandler)
    } else if let lmPackage = package as? LexicalModelKeymanPackage {
     doInstallPrompt(for: lmPackage, in: rootVC, withAssociators: [], successHandler: successHandler)
    }
  }

  public func buildKMPError(_ error: KMPError) -> UIAlertController {
    return buildSimpleAlert(title: NSLocalizedString("alert-error-title", bundle: engineBundle, comment: ""),
                            message: error.localizedDescription)
  }

  public func buildSimpleAlert(title: String, message: String, completionHandler: (() -> Void)? = nil ) -> UIAlertController {
    let alertController = UIAlertController(title: title, message: message,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: NSLocalizedString("command-ok", bundle: engineBundle, comment: ""),
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
      let errorMessage = "\(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
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
      let errorMessage = "Resource with full ID \(missingResource.description) not in package"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
      throw KMPError.resourceNotInPackage
    }

    // Resources should only be installed after Migrations have been run.
    // Otherwise, the Migrations engine may misinterpret the installed format
    // on an app's first install.
    //
    // It is possible for a KeymanEngine framework consumer to reach this point
    // without `Manager.init` having been run.
    runMigrationsIfNeeded()

    do {
      try copyWithOverwrite(from: package.sourceFolder,
                            to: Storage.active.packageDir(for: package)!)
    } catch {
      let errorMessage = "Could not create installation directory and/or copy resources: \(error)"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
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
      // Is 'internal' and only called after packages have been installed,
      // thus when the files should already be in-place.
      let message = "Could not add resource of type: \(resource.fullID.type) with ID: \(resource.id) because the resource file does not exist"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(message)
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
    let message = "Added \(resource.fullID.type) with ID: \(resource.id) and language code: \(resource.languageID)"
    os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
    SentryManager.breadcrumb(message)
  }
}
