/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-30
 *
 * Tracks the state of a package being downloaded with helpful functions
 * to derive its temporary download location, compare it to an
 * package of the same type if it exists and replace or delete depending
 * on its version and user feedback.
 */

import Foundation

@MainActor // run on the main actor as it is called from SettingsContainer
public class PackageDownload {
  //  let packageName: String
  let temporaryKmpFileLocation: URL
  let temporaryPackageLocation: URL
  let installPackageLocation: URL
  let installedPackages: [KeymanPackage]    // needed to check for existing package after download
  var packageToInstall: KeymanPackage?      // the newly downloaded package
  var packageToReplace: KeymanPackage?      // the package to replace, if it exists

  fileprivate let packageRepository: PackageRepo
  
  public init(filename: String, packageName: String, packageRepo: PackageRepo, installedPackages: [KeymanPackage]) {
    self.packageRepository = packageRepo
    self.temporaryKmpFileLocation = self.packageRepository.getDownloadUrl(for: filename)
    self.temporaryPackageLocation = self.packageRepository.getUnzipDestinationUrl(for: packageName)
    self.installPackageLocation = self.packageRepository.getInstallationUrlForPackageName(packageName: packageName)
    self.installedPackages = installedPackages
    
    // cannot be initialized until after download when packageName of new package is known
    self.packageToReplace = nil
    
    // MAC-CONFIG-TODO: should we resume a download if the app was quit or killed before completing a keyboard install?
    // if any packages are remaining from an earlier download, delete them
    self.packageRepository.cleanupTempDirectory()
  }
  
  /**
   * Indicates that a package has been downloaded and is ready to be unzipped and installed
   */
  public func packageDownloadComplete(for kmpFileUrl: URL) {
    print ("packageDownloadComplete \(kmpFileUrl)")
    
    do {
      try self.unzipDownloadedPackage(for: kmpFileUrl)
      try self.handleNewPackage()
    } catch {
      print ("package installation failed with error '\(error)' for \(kmpFileUrl)")
      // MAC-CONFIG-TODO: send notification that installation failed
    }
  }
  
  func unzipDownloadedPackage(for kmpFileUrl: URL) throws {
    try self.packageRepository.unzipKmpFile(at: kmpFileUrl, to: self.temporaryPackageLocation)
    
    // load the unzipped package from the temporary location and save a reference to it
    let newPackage = try self.packageRepository.loadSinglePackage(packageUrl: self.temporaryPackageLocation)
    self.packageToInstall = newPackage
  }
  
  /**
   * Decides whether the package should be installed.
   * - If this package is not replacing a package, then it is installed.
   * - If this package is replacing an older package, the new package replaces the old.
   * - If this package is replacing an older package, then the user is notified to confirm.
   */
  func handleNewPackage() throws {
    // first check whether this install is replacing an existing package,
    if self.checkForExistingPackage() {
      if self.replacingInstalledPackageWithEarlierVersion() {
        // check with the user before allowing a downgrade
        self.sendNotificationToConfirmPackageDowngrade()
      } else {
        try self.replaceExistingPackageWithNewPackage()
      }
    } else {
      try self.installNewPackage()
    }
  }
  
  /**
   * Check whether a package of the same name is already installed which may be replaced.
   */
  func checkForExistingPackage() -> Bool {
    var packageExists = false
    
    if let package = self.installedPackages.first(where: { $0.packageName == self.packageToInstall?.packageName }) {
      self.packageToReplace = package
      packageExists = true
    }
    return packageExists
  }
  
  func sendNotificationToConfirmPackageDowngrade() {
    NotificationCenter.default.post(
      name: .packageDowngradeRequested,
      object: nil,
      userInfo: nil
    )
  }
  
  func installNewPackage() throws {
    try self.movePackageFromTemporaryToInstalled()
    try self.deleteDownloadedKmpFile()

    NotificationCenter.default.post(
      name: .newPackageInstalled,
      object: nil,
      userInfo: nil
    )
  }

  func replaceExistingPackageWithNewPackage() throws {
    try self.deleteInstalledPackage()
    try self.deleteDownloadedKmpFile()
    try self.movePackageFromTemporaryToInstalled()
    
    NotificationCenter.default.post(
      name: .packageReplaced,
      object: nil,
      userInfo: nil
    )
  }
  
  /**
   * Clean up the downloaded .kmp file and package folder
   */
  func cancelInstallation() throws {
    try self.deleteDownloadedKmpFile()
    try self.deleteDownloadedPackage()
  }

  func deleteInstalledPackage() throws {
    try FileManager.default.removeItem(at: self.installPackageLocation)
  }

  func movePackageFromTemporaryToInstalled() throws {
    try FileManager.default.moveItem(at: self.temporaryPackageLocation, to: self.installPackageLocation)
  }
  
  func deleteDownloadedKmpFile() throws {
    try FileManager.default.removeItem(at: self.temporaryKmpFileLocation)
  }
  
  func deleteDownloadedPackage() throws {
    try FileManager.default.removeItem(at: self.temporaryPackageLocation)
  }

  func replacingInstalledPackageWithEarlierVersion() -> Bool {
    // MAC-CONFIG-TODO: implement package version comparison
    if let existingPackage = self.packageToReplace, let newPackage = self.packageToInstall {
      return existingPackage.packageVersion > newPackage.packageVersion
    } else {
      return false
    }
  }
}
