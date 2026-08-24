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
import CoreText

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
  let installedPackages: [KeymanPackage]    // needed to check for existing package after download
  let isDownload: Bool                      // if not download, then the package was opened from disk or dropped
  
  // following properties cannot be set until new package is unzipped and loaded
  public private(set) var installPackageLocation: URL?          // derived from new package name
  public private(set) var packageToInstall: KeymanPackage?      // the newly downloaded package
  public private(set) var packageToReplace: KeymanPackage?      // the package to replace, if it exists
  public private(set) var packageInstallationType: PackageInstallationType?
  
  public var packageName: String? {
    return packageToInstall?.packageName
  }

  fileprivate let packageRepository: PackageRepo
  
  public init(filename: String, packageRepo: PackageRepo, installedPackages: [KeymanPackage], isDownload: Bool) {
    self.packageRepository = packageRepo
    self.temporaryKmpFileLocation = self.packageRepository.getDownloadUrl(for: filename)
    
    //  filename minus .kmp extension
    let directoryName = filename.replacingOccurrences(of: kmpFileExtension, with: "")
    self.temporaryPackageLocation = self.packageRepository.getUnzipDestinationUrl(for: directoryName)
    self.installedPackages = installedPackages
    self.isDownload = isDownload
    
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
      // unzip to the temp directory
      try self.packageRepository.unzipKmpFile(at: kmpFileUrl, to: self.temporaryPackageLocation)
      
      // load the unzipped package from the temp directory and save a reference to it
      let package = try self.packageRepository.loadSinglePackage(packageUrl: self.temporaryPackageLocation)
      self.packageToInstall = package

      // now that the package is loaded, we can build the installation directory from the packageName
      self.installPackageLocation = self.packageRepository.buildInstallationUrlForPackageName(packageName: package.packageName)

      // now that we know what we are installing, determine the type of install
      self.packageInstallationType = self.determinePackageInstallationType()
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
   * Install all fonts found in the package (files with an extension of .ttf or .otf).
   * The package has been copied to the installation directory, so all fonts are located at `installPackageLocation`
   * If any fonts fail to install, log the error but continue to the next font
   */
  func installFontsForPackage() {
    let fileManager = FileManager.default
    
    guard let installLocation = self.installPackageLocation else {
      print("error: installPackageLocation not set when installing fonts")
      return
    }
    
    var fileUrls: [URL] = []
    do {
      fileUrls = try fileManager.contentsOfDirectory(
        at: installLocation,
        includingPropertiesForKeys: [.isDirectoryKey],
        options: [.skipsHiddenFiles]) }
    catch {
      print("error: unable to get contents of directory at \(installLocation.path) with error: \(String(describing: error))")
    }
    
    for fontUrl in fileUrls {
      let ext = fontUrl.pathExtension.lowercased()
      if ext == "ttf" || ext == "otf" {
        // if a font fails to install, log error and continue
        guard self.validateFont(at: fontUrl) else {
          print("error: the font \(fontUrl.lastPathComponent) is not valid")
          continue
        }
        do {
          try self.copyFontToFontsDirectory(at: fontUrl)
          try self.registerFontWithSystem(at: fontUrl)
        } catch {
          print("error: the font \(fontUrl.lastPathComponent) could not be installed with error: \(String(describing: error))")
        }
      }
    }
  }
  
  /**
   * Check to see whether the font appears to be valid before installing it.
   */
  func validateFont(at url: URL) -> Bool {
      guard let descriptors = CTFontManagerCreateFontDescriptorsFromURL(url as CFURL) as? [CTFontDescriptor] else {
          return false
      }
      return !descriptors.isEmpty
  }

  /**
   * Copy the font to the fonts directory and return the URL for its new location.
   * If a font of the same name already exists, then remove it before copying the new one.
   */
  func copyFontToFontsDirectory(at fontUrl: URL) throws {
    let fontsDirectory = KeymanPaths.getFontsDirectory
    let fontDestinationUrl = fontsDirectory.appendingPathComponent(fontUrl.lastPathComponent)
    let fileManager = FileManager.default
    
    // remove the font from the fonts directory just in case it is an old one
    if fileManager.fileExists(atPath: fontDestinationUrl.path) {
      print("removed existing font: \(fontDestinationUrl.lastPathComponent)")
      try? fileManager.removeItem(at: fontDestinationUrl)
    }
    
    try fileManager.copyItem(at: fontUrl, to: fontDestinationUrl)
    print("added font: \(fontDestinationUrl.lastPathComponent)")
  }

  /**
   * Register the font in the macOS font manager.
   * The scope is specified as `CTFontManagerScope.user` which makes the font available to any app
   * and causes it to appear in the macOS Font Book application.
   */
  func registerFontWithSystem(at fontUrl: URL) throws {
    let dispatchGroup = DispatchGroup()
    var registrationError: Error?
    
    // pause current thread until background tasks are complete
    dispatchGroup.enter()
    
    // CTFontManagerRegisterFontURLs returns void -- errors must be captured in the block
    CTFontManagerRegisterFontURLs([fontUrl] as CFArray, .user, true) { (errors, done) -> Bool in
      let errorArray = errors as? [CFError] ?? []
      
      if !errorArray.isEmpty {
        
        for cfError in errorArray {
          let errorCode = CFErrorGetCode(cfError)
          
          // code 105 = kCTFontManagerErrorAlreadyRegistered
          // It is safe to ignore because the font is
          if errorCode == 105 {
            print("font \(fontUrl.lastPathComponent) is already registered.")
            continue
          }

          // if it's any other error, capture it to throw later
          print("registerFontWithSystem failed for \(fontUrl.lastPathComponent), error: \(String(describing: cfError))")
          registrationError = InstallPackageError.fontRegistrationError
        }

        dispatchGroup.leave()
        return false // stop registration execution
      }
      
      if done {
        dispatchGroup.leave()
      }
      return true // Continue processing
    }
    
    // wait synchronously for CoreText to finish processing the font file
    dispatchGroup.wait()
    
    // throw an error out to your installation pipeline if registration failed
    if let error = registrationError {
      throw error
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
   * Install the newly downloaded package (no existing package to replace)
   */
  func installNewPackage() throws {
    if (self.isDownload) {
      do {
        try self.deleteDownloadedKmpFile()
      } catch {
        print("installNewPackage failed to delete downloaded .kmp file: \(self.temporaryKmpFileLocation.lastPathComponent)")
      }
    }

    try self.movePackageFromTemporaryToInstalled()
    try self.installFontsForPackage()
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
    try self.installFontsForPackage()
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
    if let installLocation = self.installPackageLocation {
      try FileManager.default.removeItem(at: installLocation)
    }
  }
  
  /**
   * Move the downloaded package into the keyman packages directory.
   */
  func movePackageFromTemporaryToInstalled() throws {
    if let installLocation = self.installPackageLocation {
      try FileManager.default.moveItem(at: self.temporaryPackageLocation, to: installLocation)
      
      // Update the KeymanPackage object with its new location
      if let package = self.packageToInstall {
        package.sourceDirectoryUrl = installLocation
      }
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
