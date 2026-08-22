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
  
  public var prompt: LocalizedStringResource {
    switch self {
    case .newPackage(let packageName):               
      return "The package '\(packageName)' is ready to install"
    case .replaceSameVersionPackage(let packageName):
      return "The package '\(packageName)' is ready to re-install"
    case .replaceOlderPackage(let packageName, let existingVersion, let newVersion):
      return "The package '\(packageName)' is ready to update from version \(existingVersion) to \(newVersion)"
    case .replaceNewerPackage(let packageName, let existingVersion, let newVersion):
      return "The package '\(packageName)' is ready to downgrade from version \(existingVersion) to \(newVersion)"
    }
  }
}

@MainActor // run on the main actor as it is called from SettingsContainer
public class PackageInstallHelper: Identifiable {
  public let id = UUID()
  public let temporaryKmpFileLocation: URL
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
      try self.unzipAndLoadPackage(for: kmpFileUrl)
    } catch {
      self.cleanupFailedInstallation()
      print ("package installation failed with error '\(error)' for \(kmpFileUrl)")
      throw error
    }
  }
  
  /**
   * Install the new package and replace existing package if necessary
   */
  public func installPackage() throws {
    print ("installPackage \(self.packageToInstall?.packageName ?? "unknown package")")

    // prepareToInstall will always set this
    guard let installationType = self.packageInstallationType else {
      print("error: installationType not set before call to installPackage")
      throw InstallPackageError.internalError
    }
    
    switch installationType {
    case .newPackage:
      try self.installNewPackage()
    case .replaceSameVersionPackage, .replaceNewerPackage, .replaceOlderPackage:
      try self.replaceExistingPackageWithNewPackage()
    }
  }

  /**
   * Unzip and load the downloaded package
   */
  func unzipAndLoadPackage(for kmpFileUrl: URL) throws {
    // unzip to the temp directory
    try self.packageRepository.unzipKmpFile(at: kmpFileUrl, to: self.temporaryPackageLocation)
    
    // load the unzipped package from the temp directory and save a reference to it
    self.packageToInstall = try self.packageRepository.loadSinglePackage(packageUrl: self.temporaryPackageLocation)
    
    // now that we know what we are installing, determine the type of install
    self.packageInstallationType = self.determinePackageInstallationType()
  }
  
  /**
   * Decides what type of package installation this is:
   * - a new package
   * - an update of an existing package
   * - a downgrade of an existing package
   */
  func determinePackageInstallationType() -> PackageInstallationType {
    let packageAlreadyInstalled = self.checkForExistingPackage()
    var installationType: PackageInstallationType = .newPackage("unknown package")
    
    // If there is no new package, return bogus value of .newPackage.
    // Without a package, the installation will fail elsewhere and the
    // type of installation is completely irrelevant.
    guard let newPackage = self.packageToInstall else {
      print("error: packageToInstall not set when determining package installation type")
      return installationType
    }
    
    if !packageAlreadyInstalled {
      installationType =  PackageInstallationType.newPackage(newPackage.packageName)
    } else {
      if let installedPackage = self.packageToReplace {
        let newVersion = newPackage.packageVersion
        let existingVersion = installedPackage.packageVersion
        
        let comparisonResult = newVersion.compare(existingVersion, options: .numeric)
        
        if comparisonResult == .orderedAscending {
          print("package downgrade: new version is older than existing version")
          installationType =  PackageInstallationType.replaceNewerPackage(newPackage.packageName, existingVersion, newVersion)
        } else if comparisonResult == .orderedDescending {
          print("package upgrade: new version is newer than existing version")
          installationType = PackageInstallationType.replaceOlderPackage(newPackage.packageName, existingVersion, newVersion)
        } else {
          print("new and existing package versions are identical")
          installationType = PackageInstallationType.replaceSameVersionPackage(newPackage.packageName)
        }
      }
    }
    
    return installationType
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
}
