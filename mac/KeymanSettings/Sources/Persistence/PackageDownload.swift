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

public class PackageDownload {
  //  let packageName: String
  let temporaryKmpFileLocation: URL
  let temporaryPackageLocation: URL
  let installPackageLocation: URL
  let packageToReplace: KeymanPackage?
  var packageToInstall: KeymanPackage?
  fileprivate let packageRepository: PackageRepo
  
  public init(filename: String, packageName: String, packageRepo: PackageRepo, replacing existingPackage: KeymanPackage? = nil) {
    self.packageRepository = packageRepo
    self.temporaryKmpFileLocation = self.packageRepository.getDownloadUrl(for: filename)
    self.temporaryPackageLocation = self.packageRepository.getUnzipDestinationUrl(for: packageName)
    self.installPackageLocation = self.packageRepository.getInstallationUrlForPackageName(packageName: packageName)
    self.packageToReplace = existingPackage
  }
  
  /**
   * Indicates that a package has been downloaded and specifies where it is downloaded,
   * and, optionally, the package (of the same type) that it is replacing.
   * In response, this function unzips the package to the temporary location and
   * decides whether the package should be installed.
   * - If this package is not replacing a package, then it is installed.
   * - If this package is replacing an older package, the new package replaces the old.
   * - If this package is replacing an older package, then the user is notified to confirm.
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
  
  func handleNewPackage() throws {
    // if this install is replacing an existing package,
    // check with the user before allowing a downgrade
    if let _ = self.packageToReplace {
      if self.replacingInstalledPackageWithEarlierVersion() {
        self.sendNotificationToConfirmPackageDowngrade()
      } else {
        try self.replaceExistingPackageWithNewPackage()
      }
    } else {
      try self.installNewPackage()
    }
  }
  
  func sendNotificationToConfirmPackageDowngrade() {
    NotificationCenter.default.post(
      name: .packageDowngradeRequested,
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
  
  func installNewPackage() throws {
    try FileManager.default.moveItem(at: self.temporaryPackageLocation, to: self.installPackageLocation)
    try self.deleteDownloadedKmpFile()

    NotificationCenter.default.post(
      name: .newPackageInstalled,
      object: nil,
      userInfo: nil
    )
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
  
  func replacingInstalledPackageWithEarlierVersion() -> Bool {
    // MAC-CONFIG-TODO: implement package version comparison
    if let existingPackage = self.packageToReplace, let newPackage = self.packageToInstall {
      return existingPackage.packageVersion > newPackage.packageVersion
    } else {
      return false
    }
  }
}
