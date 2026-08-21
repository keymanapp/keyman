/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-30
 *
 * Tracks the state of a package being installed with functions
 * to derive its temporary install location, compare it to a
 * package of the same type if it exists and replace or delete depending
 * on its version and user feedback.
 */

import Foundation

public enum PackageInstallationType {
  case newPackage(String)
  case replaceSameVersionPackage(String)
  case replaceOlderPackage(String, String, String)
  case replaceNewerPackage(String, String, String)
  case packageNotFound
  
  public var prompt: LocalizedStringResource {
    switch self {
    case .newPackage(let packageName):               
      return "The package \(packageName) is ready to install"
    case .replaceSameVersionPackage(let packageName):
      return "The package \(packageName) is ready to re-install"
    case .replaceOlderPackage(let packageName, let existingVersion, let newVersion):
      return "The package \(packageName) is ready to update from version \(existingVersion) to \(newVersion)"
    case .replaceNewerPackage(let packageName, let existingVersion, let newVersion):
      return "The package \(packageName) is ready to downgrade from version \(existingVersion) to \(newVersion)"
    case .packageNotFound:
      return "No package to install"
    }
  }
}

@MainActor // run on the main actor as it is called from SettingsContainer
public class PackageInstallHelper: Identifiable {
  public let id = UUID()
//  public var installationError: LocalizedError?
//  public var errorMessage: String?
  let temporaryKmpFileLocation: URL
  let temporaryPackageLocation: URL
  let installPackageLocation: URL
  let installedPackages: [KeymanPackage]    // needed to check for existing package after download
  let isDownload: Bool                      // if not download, then the package was opened from disk or dropped
  public private(set) var packageToInstall: KeymanPackage?      // the newly downloaded package
  public private(set) var packageToReplace: KeymanPackage?      // the package to replace, if it exists
  public private(set) var packageInstallationType: PackageInstallationType?
  
  public var packageName: String? {
    return packageToInstall?.packageName
  }

  fileprivate let packageRepository: PackageRepo
  
  public init(filename: String, packageName: String, packageRepo: PackageRepo, installedPackages: [KeymanPackage], isDownload: Bool) {
    self.packageRepository = packageRepo
    self.temporaryKmpFileLocation = self.packageRepository.getDownloadUrl(for: filename)
    self.temporaryPackageLocation = self.packageRepository.getUnzipDestinationUrl(for: packageName)
    self.installPackageLocation = self.packageRepository.buildInstallationUrlForPackageName(packageName: packageName)
    self.installedPackages = installedPackages
    self.isDownload = isDownload
    
    // cannot be initialized until after download when packageName of new package is known
    self.packageToReplace = nil
    
    // if any packages are remaining from an earlier download, delete them
    self.packageRepository.cleanupTempDirectory()
  }
  
  /**
   * Indicates that a package has been downloaded and can be prepared for installation
   */
  public func packageDownloadComplete(for kmpFileUrl: URL) throws {
    print ("packageDownloadComplete \(kmpFileUrl)")
    
    try self.prepareToInstall(for: kmpFileUrl)
  }
  
  /**
   * Indicates that a package is ready to be unzipped and loaded
   */
  public func prepareToInstall(for kmpFileUrl: URL) throws {
    print ("prepareToInstall \(kmpFileUrl)")
    
    do {
      try self.unzipPackage(for: kmpFileUrl)
    } catch {
      self.cleanupFailedInstallation()
      print ("package installation failed with error '\(error)' for \(kmpFileUrl)")
      throw error
    }
    
    self.packageInstallationType = self.determinePackageInstallationType()
  }
  
  /**
   * Install the unzipped package
   */
  public func install() throws {
    print ("install \(self.packageToInstall?.packageName ?? "unknown package")")
    
    do {
      try self.handleNewPackage()
    } catch {
      self.cleanupFailedInstallation()
      print ("package installation failed with error '\(self.packageToInstall?.packageName ?? "unknown package")")
      throw error
    }
  }
  
  /**
   * Install the new package and replace existing package if necessary
   */
  public func installPackage() throws {
    print ("installPackage \(self.packageToInstall?.packageName ?? "unknown package")")

    guard let installationType = self.packageInstallationType else { return }
    
    switch installationType {
    case .newPackage:
      try self.installNewPackage()
    case .replaceSameVersionPackage, .replaceNewerPackage, .replaceOlderPackage:
      try self.replaceExistingPackageWithNewPackage()
    case .packageNotFound:
      throw DropKmpError.installFailed("unknown package installation type")
    }
  }


  /**
   * Unzip and load the downloaded package
   */
  func unzipPackage(for kmpFileUrl: URL) throws {
    try self.packageRepository.unzipKmpFile(at: kmpFileUrl, to: self.temporaryPackageLocation)
    
    // load the unzipped package from the temporary location and save a reference to it
    let newPackage = try self.packageRepository.loadSinglePackage(packageUrl: self.temporaryPackageLocation)
    self.packageToInstall = newPackage
    
    
  }
  
  /**
   * Decides what type of package installation this is:
   * - a new package
   * - an update of an existing package
   * - a downgrade of an existing package
   */
  func determinePackageInstallationType() -> PackageInstallationType {
    let packageAlreadyInstalled = self.checkForExistingPackage()
    
    guard let newPackage = self.packageToInstall else {
      return PackageInstallationType.packageNotFound
    }
    
    if !packageAlreadyInstalled {
      return PackageInstallationType.newPackage(newPackage.packageName)
    } else {
      if let installedPackage = self.packageToReplace {
        let newVersion = newPackage.packageVersion
        let existingVersion = installedPackage.packageVersion
        
        let comparisonResult = newVersion.compare(existingVersion, options: .numeric)
        
        if comparisonResult == .orderedAscending {
          print("package downgrade: new version is older than existing version")
          return PackageInstallationType.replaceNewerPackage(newPackage.packageName, existingVersion, newVersion)
        } else if comparisonResult == .orderedDescending {
          print("package upgrade: new version is newer than existing version")
          return PackageInstallationType.replaceOlderPackage(newPackage.packageName, existingVersion, newVersion)
        } else {
          print("new and existing package versions are identical")
          return PackageInstallationType.replaceSameVersionPackage(newPackage.packageName)
        }
      }
    }

    return PackageInstallationType.packageNotFound
  }
  
  /**
   * Decides whether the package should be installed.
   * - If this package is not replacing a package, then it is installed.
   * - If this package is replacing an older package, the new package replaces the old.
   * - If this package is replacing a newer package, then the user is notified to confirm.
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
    NotificationCenter.default.post(name: .packageDowngradeRequested, object: nil)
  }
  
  /**
   * Install the newly downloaded package (no existing package to replace)
   */
  func installNewPackage() throws {
    try self.movePackageFromTemporaryToInstalled()
    if (self.isDownload) {
      do {
        try self.deleteDownloadedKmpFile()
      } catch {
        print("installNewPackage failed to delete downloaded .kmp file: \(self.temporaryKmpFileLocation.lastPathComponent)")
      }
    }
    
    NotificationCenter.default.post(name: .newPackageInstalled, object: nil)
  }
  
  /**
   * Replace the existing installed package with the newly download package
   */
  func replaceExistingPackageWithNewPackage() throws {
    try self.deleteInstalledPackage()
    if (self.isDownload) {
      do {
        try self.deleteDownloadedKmpFile()
      } catch {
        print("replaceExistingPackageWithNewPackage failed to delete downloaded .kmp file: \(self.temporaryKmpFileLocation.lastPathComponent)")
      }
    }
    try self.movePackageFromTemporaryToInstalled()
    
    NotificationCenter.default.post(name: .packageReplaced, object: nil)
  }
  
  /**
   * Clean up the downloaded .kmp file and package folder
   */
  func cleanupFailedInstallation() {
    // we only have a .kmp file in the temp directory for downloads
    if (self.isDownload) {
      do {
        try self.deleteDownloadedKmpFile()
      } catch {
        print("cleanupFailedInstallation did not delete downloaded .kmp file: \(self.temporaryKmpFileLocation.lastPathComponent)")
      }
    }
    do {
      try self.deleteUnzippedPackage()
    } catch {
      print("cleanupFailedInstallation did not delete downloaded package: \(self.temporaryPackageLocation.lastPathComponent)")
    }
  }
  
  /**
   * Delete the existing installed package that matches the downloaded package
   */
  func deleteInstalledPackage() throws {
    try FileManager.default.removeItem(at: self.installPackageLocation)
  }
  
  /**
   * Move the downloaded package into the keyman packages directory.
   */
  func movePackageFromTemporaryToInstalled() throws {
    try FileManager.default.moveItem(at: self.temporaryPackageLocation, to: self.installPackageLocation)
    
    // Update the KeymanPackage object with its new location
    if let package = self.packageToInstall {
      package.sourceDirectoryUrl = self.installPackageLocation
    }
  }
  
  /**
   * Delete the downloaded .kmp file from the temp directory
   */
  func deleteDownloadedKmpFile() throws {
    try FileManager.default.removeItem(at: self.temporaryKmpFileLocation)
  }
  
  /**
   * Delete the unzipped package in the temp directory
   */
  func deleteUnzippedPackage() throws {
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
