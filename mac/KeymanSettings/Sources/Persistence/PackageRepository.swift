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
  
  public init() {
    self.pathUtil = KeymanPaths()
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
   */
  public func createKeyman19SharedDataDirectories() {
    if let keyboardDirectory = pathUtil.keyman19PackagesDirectory {
      
      do {
        // create the directory if it doesn't already exist
        if !FileManager.default.fileExists(atPath: keyboardDirectory.path) {
          try FileManager.default.createDirectory(at: keyboardDirectory, withIntermediateDirectories: true, attributes: nil)
          //          ConfigLogger.shared.testLogger.debug("Created directory: \(keyboardDirectory.path)")
          print("Created directory: \(keyboardDirectory.path)")
        } else {
          print("Directory already exists: \(keyboardDirectory.path)")
        }
      } catch {
        print("Error creating directory: \(error.localizedDescription)")
      }
    } else {
      print("Unable to create directory, Group Container URL not found.")
    }
  }
  
  // MAC-CONFIG-TODO: delete
  /**
   * for group container testing purposes to check directory access
   */
  func writeTestFileToContainer() {
    if let keyboardDirectory = self.pathUtil.keyman19PackagesDirectory {
      print("About to write to: \(keyboardDirectory.path)")
      
      do {
        // Example: Save a file to the shared directory
        let fileURL = keyboardDirectory.appendingPathComponent("bogus-kmp.txt")
        let data = "Not a .kmp file".data(using: .utf8)
        try data?.write(to: fileURL)
        print("Wrote data to shared file: \(fileURL.path)")
        
      } catch {
        print("Error writing file to shared directory: \(error.localizedDescription)")
      }
    } else {
      print("App Group container URL not found. Check your entitlements and provisioning profiles.")
    }
  }
 
  /**
   * get the url for where the specified package should be downloaded
   */
  public func getDownloadUrlForPackageName(packageName: String) -> URL? {
    var downloadFileUrl: URL? = nil
    
    if let packagesUrl = self.pathUtil.keyman19PackagesDirectory {
      downloadFileUrl = packagesUrl.appendingPathComponent(packageName)
    }
    
    return downloadFileUrl
  }

  /**
   * install keyboard at specified URL
   */
  public func installPackage(packageUrl: URL) throws -> URL? {
    print ("install package \(packageUrl)")
    var destinationUrl: URL? = nil
    
    let fileManager = FileManager.default
    
    do {
      // set destination to full path but without the .kmp extension
      let packageDestinationUrl = packageUrl.deletingPathExtension()
      
      try fileManager.unzipItem(at: packageUrl, to: packageDestinationUrl)
      destinationUrl = packageDestinationUrl
      print("Successfully unzipped the file!")
    } catch {
      print("Extraction failed: \(error.localizedDescription)")
      throw InstallPackageError.unzipError
    }
    
    return destinationUrl
  }

  /**
   * Check to see whether the shared Keyman data directory exists under 'Library/Group Containers/'
   */
  public func keyman19SharedDataDirectoryExists() -> Bool {
    guard let packagesUrl = self.pathUtil.keyman19PackagesDirectory else {
      return false
    }
    
    return self.directoryExistsAtPath(directoryUrl: packagesUrl)
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
    guard let packagesUrl = self.pathUtil.keyman19PackagesDirectory else {
      return []
    }
    
    return readPackageSource(packageDirectoryUrl: packagesUrl)
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
