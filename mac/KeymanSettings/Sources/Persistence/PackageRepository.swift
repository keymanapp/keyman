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

public class PackageRepository: PackageRepo {
  fileprivate let packageFileName = "kmp.json"
  fileprivate let pathUtil: KeymanPaths
  
  public init() {
    self.pathUtil = KeymanPaths()
  }
  
  /**
   * after reading the Keyman packages from disk, wrap each package as a `KeymanPackage` object
   */
  public func loadPackages() -> [KeymanPackage] {
    var installedPackages: [KeymanPackage] = []
    let packageSourceArray = self.readKeymanPackagesForKeyman19()
    
    // create a KeymanPackage object for each PackageSource object and insert it in the array if it is valid
    for source in packageSourceArray {
      let package = KeymanPackage(packageSource: source)
      if (package.validate()) {
        installedPackages.append(package)
      } else {
        print("package '\(source.packageName)' is not valid")
      }
    }
    
    return installedPackages
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
  
  // TODO-MAC-CONFIG: delete
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
          if let packageSource =  readPackageFromDirectory(packageDirectoryUrl: itemUrl) {
            packages.append(packageSource)
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
  func readPackageFromDirectory(packageDirectoryUrl: URL) -> PackageSource? {
    var packageSource: PackageSource?
    let lastPathComponent = packageDirectoryUrl.lastPathComponent
    let kmpJsonFileUrl = packageDirectoryUrl.appendingPathComponent(packageFileName)
    
    if FileManager.default.fileExists(atPath: kmpJsonFileUrl.path) {
      if let source = readPackage(packageDirectoryUrl: packageDirectoryUrl, kmpFileUrl: kmpJsonFileUrl) {
        packageSource = source
      }
    } else {
      print(" ** the package directory \(lastPathComponent) does not contain the file 'kmp.json')")
    }
    
    return packageSource
  }
  
  /**
   * read and parse the kmp.json file at the specified URL
   */
  func readPackage(packageDirectoryUrl: URL, kmpFileUrl: URL) -> PackageSource? {
    var packageSource: PackageSource?
    do {
      let jsonData = try Data(contentsOf: kmpFileUrl, options: .mappedIfSafe)
      var source: PackageSource = try JSONDecoder().decode(PackageSource.self, from: jsonData)
      
      print("package name: \(source.packageName)")
      // save the keyboard directory and path of the kmp.json for this keyboard
      source.directoryUrl = packageDirectoryUrl
      source.kmpJsonFileUrl = kmpFileUrl
      packageSource = source
    } catch {
      print("\(error.localizedDescription)")
    }
    return packageSource
  }
}
