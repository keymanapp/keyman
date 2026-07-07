/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-30
 *
 * Tracks the state of a package being downloaded with functions
 * to derive its temporary download location, compare it to a
 * package of the same type if it exists and replace or delete depending
 * on its version and user feedback.
 */

import Foundation

@MainActor // run on the main actor as it is called from SettingsContainer
public class PackageDownload {
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
      // MAC-CONFIG-TODO: handle error
      // send notification that installation failed?
    }
  }
  
  /**
   * Unzip the and load the downloaded package
   */
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
  
  /**
   * Send a notification that an attempt to downgrade a package has been detected
   */
  func sendNotificationToConfirmPackageDowngrade() {
    NotificationCenter.default.post(
      name: .packageDowngradeRequested,
      object: nil,
      userInfo: nil
    )
  }
  
  /**
   * Install the newly downloaded package (no existing package to replace)
   */
  func installNewPackage() throws {
    try self.movePackageFromTemporaryToInstalled()
    try self.deleteDownloadedKmpFile()
    
    NotificationCenter.default.post(
      name: .newPackageInstalled,
      object: nil,
      userInfo: nil
    )
  }
  
  /**
   * Replace the existing installed package with the newly download package
   */
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
  
  /**
   * Delete the existing installed package that matches the downloaded package
   */
  func deleteInstalledPackage() throws {
    try FileManager.default.removeItem(at: self.installPackageLocation)
  }
  
  /**
   * Move the downloaded package into the keyman packages directory
   */
  func movePackageFromTemporaryToInstalled() throws {
    try FileManager.default.moveItem(at: self.temporaryPackageLocation, to: self.installPackageLocation)
  }
  
  /**
   * Delete the downloaded .kmp file from the temp directory
   */
  func deleteDownloadedKmpFile() throws {
    try FileManager.default.removeItem(at: self.temporaryKmpFileLocation)
  }
  
  /**
   * Delete the downloaded package from the temp directory
   */
  func deleteDownloadedPackage() throws {
    try FileManager.default.removeItem(at: self.temporaryPackageLocation)
  }
  
  /**
   * Determine whether the new package is older than the currently installed package
   */
  func replacingInstalledPackageWithEarlierVersion() -> Bool {
    var downgrade = false
    
    guard let installedVersion = self.packageToReplace?.packageVersion,
          let newVersion = self.packageToInstall?.packageVersion else {
      return false
    }
    
    let comparisonResult = newVersion.compare(installedVersion, options: .numeric)
    
    if comparisonResult == .orderedAscending {
      // downgrade detected
      downgrade = true
      print("downgrade: new version is older than installed version")
    } else if comparisonResult == .orderedDescending {
      print("upgrade: new version is newer than installed version")
    } else {
      print("new and installed versions are identical")
    }
    
    return downgrade
  }
}
