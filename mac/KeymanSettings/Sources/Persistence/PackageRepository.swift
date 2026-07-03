/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * PackageRepository is responsible for reading, writing and removing
 * Keyman data on disk.
 *
 */

import Foundation

enum LoadPackageError: Error {
  case containsNoFiles
  case containsNoKeyboards
  case kmpJsonFileUnreadable
  case kmpJsonFileNotFound
  case missingKeyboardName
  case missingKeyboardId
  case missingKeyboardVersion
  case missingKmxFile
}

enum InstallPackageError: Error {
  case invalidUrl
  case unzipError
}

// Conform to LocalizedError to provide the description
extension LoadPackageError: LocalizedError {
  var errorDescription: String? {
    switch self {
    case .containsNoFiles:
      return NSLocalizedString("The package contains no files.", comment: "")
    case .containsNoKeyboards:
      return NSLocalizedString("The package contains no keyboards", comment: "")
    case .kmpJsonFileUnreadable:
      return NSLocalizedString("The package's kmp.json file could not be parsed", comment: "")
    case .kmpJsonFileNotFound:
      return NSLocalizedString("The package's kmp.json file was not found", comment: "")
    case .missingKeyboardName:
      return NSLocalizedString("A keyboard in the package has no name", comment: "")
    case .missingKeyboardId:
      return NSLocalizedString("A keyboard in the package has no id", comment: "")
    case .missingKeyboardVersion:
      return NSLocalizedString("A keyboard in the package has no version", comment: "")
    case .missingKmxFile:
      return NSLocalizedString("A keyboard in the package has no corresponding KMX file", comment: "")
    }
  }
}

public class PackageRepository: PackageRepo {
  fileprivate let packageFileName = "kmp.json"
  fileprivate let pathUtil: KeymanPaths
  
  public init() throws {
    self.pathUtil = try KeymanPaths()
    
    try self.createKeyman19SharedDataDirectoriesIfNeeded()
  }
  
  /**
   * Load the Keyman packages from disk and wrap each package as a `KeymanPackage` object
   * If the `KeymanPackage` passes validation, then add it to the `installedPackages` array.
   *
   */
  public func loadAllPackages() -> [KeymanPackage] {
    var installedPackages: [KeymanPackage] = []
    let packageSourceArray = self.readKeymanPackagesForKeyman19()
    
    // create a KeymanPackage object for each PackageSource object and insert it in the array if it is valid
    for source in packageSourceArray {
      let package = KeymanPackage(packageSource: source)
      do {
        try package.validate()
      } catch {
        print("** package '\(source.packageName)' is not valid: \(error.localizedDescription)")
      }
      // only install packages that pass validation
      installedPackages.append(package)
    }
    
    return installedPackages
  }
  
  /**
   * Load the single package from disk and wrap it as a `KeymanPackage` object
   * If the `KeymanPackage` passes validation, then add it to the `installedPackages` array.
   *
   */
  public func loadSinglePackage(packageUrl: URL) throws -> KeymanPackage {
    print("loadSinglePackage from url: \(packageUrl)")
    guard let source =  try readPackageFromDirectory(packageDirectoryUrl: packageUrl) else { throw InstallPackageError.invalidUrl }
      
    let package = KeymanPackage(packageSource: source)
    try package.validate()
    return package
  }

  /**
   * delete the package from disk
   */
  public func deletePackage(package: KeymanPackage) {
    print("deleting package: \(package.sourceDirectoryUrl)")
    do {
      try FileManager.default.removeItem(at: package.sourceDirectoryUrl)
      print("deleted package: \(package.sourceDirectoryUrl)")
    } catch {
      print("could not delete directory: \(error.localizedDescription)")
    }
  }
  
  /**
   * Creates the directory tree where packages are stored under the standard 'Group Containers' directory
   * Also creates the temp directory used for keyboard installation
   */
  public func createKeyman19SharedDataDirectoriesIfNeeded() throws {
    let packageDirectory = pathUtil.keyman19PackagesDirectory
    let packageTempDirectory = pathUtil.keyman19TempDirectory

    // create the keyman-packages directory if it doesn't already exist
    if !FileManager.default.fileExists(atPath: packageDirectory.path) {
      try FileManager.default.createDirectory(at: packageDirectory, withIntermediateDirectories: true, attributes: nil)
      print("Created directory: \(packageDirectory.path)")
    } else {
      print("Directory already exists: \(packageDirectory.path)")
    }

    // create the temp directory if it doesn't already exist
    if !FileManager.default.fileExists(atPath: packageTempDirectory.path) {
      try FileManager.default.createDirectory(at: packageTempDirectory, withIntermediateDirectories: true, attributes: nil)
      print("Created directory: \(packageTempDirectory.path)")
    } else {
      print("Directory already exists: \(packageTempDirectory.path)")
    }
  }
  
  /**
   * get the url to where the specified kmp file should be downloaded
   */
  public func getDownloadUrl(for kmpFilename: String) -> URL {
    return self.pathUtil.keyman19TempDirectory.appendingPathComponent(kmpFilename)
  }

  /**
   * get the url to where the specified package should initially be unzipped
   */
  public func getUnzipDestinationUrl(for packageName: String) -> URL {
    return self.pathUtil.keyman19TempDirectory.appendingPathComponent(packageName)
  }
  /**
   * get the url to where the specified package should be installed
   */
  public func getInstallationUrlForPackageName(packageName: String) -> URL {
    return self.pathUtil.keyman19PackagesDirectory.appendingPathComponent(packageName)
  }

  /**
   * install keyboard at specified URL
   */
  public func unzipKmpFile(at kmpFileUrl: URL, to packageDestinationUrl: URL) throws {
    do {
      try FileManager.default.unzipItem(at: kmpFileUrl, to: packageDestinationUrl)
      print("Successfully unzipped the file!")
    } catch {
      print("Extraction failed: \(error.localizedDescription)")
      throw InstallPackageError.unzipError
    }
  }

  /**
   * Check to see whether the shared Keyman data directory exists under 'Library/Group Containers/'
   */
  public func keyman19SharedDataDirectoryExists() -> Bool {
    return self.directoryExistsAtPath(directoryUrl: self.pathUtil.keyman19PackagesDirectory)
  }
  
  /**
   * returns true if a directory exists at the specified URL
   */
  func directoryExistsAtPath(directoryUrl: URL) -> Bool {
    var isDirectory: ObjCBool = false
    let exists = FileManager.default.fileExists(atPath: directoryUrl.path, isDirectory: &isDirectory)
    return exists && isDirectory.boolValue
  }
  
  /**
   * read packages at Keyman 19 location, inside Group Containers directory
   */
  func readKeymanPackagesForKeyman19() -> [PackageSource] {
    return readPackageSource(packageDirectoryUrl: self.pathUtil.keyman19PackagesDirectory)
  }
  
  /**
   * loop through all the sub-directories in the packages directory and try to read them as packages
   */
  func readPackageSource(packageDirectoryUrl: URL) -> [PackageSource] {
    var packages: [PackageSource] = []
    
    do {
      // Get the URLs for all items in the directory that are not hidden
      let directoryContents = try FileManager.default.contentsOfDirectory(
        at: packageDirectoryUrl,
        includingPropertiesForKeys: nil,
        options: [.skipsHiddenFiles]
      )
      
      for itemUrl in directoryContents {
        // if the item is a directory, then attempt to read it as a keyboard package
        if (itemUrl.hasDirectoryPath) {
          do {
            if let packageSource =  try readPackageFromDirectory(packageDirectoryUrl: itemUrl) {
              packages.append(packageSource)
            }
          } catch let error as LoadPackageError {
            print("** package at \(itemUrl) could not be loaded: \(error.localizedDescription)")
          }
        }
      }
    } catch {
      print("Failed to read directory: \(error.localizedDescription)")
    }
    
    print("\(packages.count) packages read")
    return packages
  }
  
  /**
   * check the specified directory for the kmp.json file and read it if it exists
   */
  func readPackageFromDirectory(packageDirectoryUrl: URL) throws -> PackageSource? {
    print("readPackageFromDirectory from url: \(packageDirectoryUrl)")
    var packageSource: PackageSource? = nil
    let kmpJsonFileUrl = packageDirectoryUrl.appendingPathComponent(packageFileName)
    
    if !FileManager.default.fileExists(atPath: kmpJsonFileUrl.path) {
      throw LoadPackageError.kmpJsonFileNotFound
    }
    
    // use try without do block
    // if an error occurs, it will not be handled but propagated to caller
    if let source = try readPackage(packageDirectoryUrl: packageDirectoryUrl, kmpFileUrl: kmpJsonFileUrl) {
      packageSource = source
    }
    
    return packageSource
  }
  
  /**
   * read and parse the kmp.json file at the specified URL
   */
  func readPackage(packageDirectoryUrl: URL, kmpFileUrl: URL) throws -> PackageSource? {
    var packageSource: PackageSource?
    do {
      let jsonData = try Data(contentsOf: kmpFileUrl, options: .mappedIfSafe)
      var source: PackageSource = try JSONDecoder().decode(PackageSource.self, from: jsonData)
      
      print("readPackage, packageName: \(source.packageName)")
      // save the keyboard directory and path of the kmp.json for this keyboard
      source.directoryUrl = packageDirectoryUrl
      source.kmpJsonFileUrl = kmpFileUrl
      packageSource = source
    } catch let error as LoadPackageError {
      // if we encounter a LoadPackageError, propagate it
      throw error
    } catch {
      // otherwise convert the error to a LoadPackageError error
      print("readPackage error: \(error.localizedDescription)")
      throw LoadPackageError.kmpJsonFileUnreadable
    }
    return packageSource
  }
}
