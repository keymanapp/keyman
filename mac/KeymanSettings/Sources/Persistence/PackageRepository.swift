/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * DataRepository is responsible for reading, writing and removing
 * Keyman data on disk.
 *
 */

import Foundation

public struct PackageRepository {
  fileprivate let packageFileName = "kmp.json"
  fileprivate let pathUtil: KeymanPaths
  
  public init() {
    self.pathUtil = KeymanPaths()
  }
  
  func loadPackages() -> [KeymanPackage] {
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
   * Creates the directory tree where keyboards are stored under the standard 'Group Containers' directory
   */
  func createKeyman19SharedDataDirectories() {
    if let keyboardDirectory = pathUtil.keyman19KeyboardsDirectory {
      
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
  
  /**
   * for group container testing purposes to check directory access
   */
  public func writeTestFileToContainer() {
    if let keyboardDirectory = self.pathUtil.keyman19KeyboardsDirectory {
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
    guard let keyboardsUrl = self.pathUtil.keyman19KeyboardsDirectory else {
      return false
    }
    
    return self.directoryExistsAtPath(directoryUrl: keyboardsUrl)
  }
  
  /**
   * returns true if a directory exists at the specified URL
   */
  func directoryExistsAtPath(directoryUrl: URL) -> Bool {
      var isDirectory: ObjCBool = false
    let exists = FileManager.default.fileExists(atPath: directoryUrl.path, isDirectory: &isDirectory)
      return exists && isDirectory.boolValue
  }

  public func readKeymanPackagesForKeyman19() -> [PackageSource] {
    guard let keyboardsUrl = self.pathUtil.keyman19KeyboardsDirectory else {
      return []
    }

    return readKeyboardPackageSource(keyboardDirectoryUrl: keyboardsUrl)
  }

  public func readKeyboardPackageSource(keyboardDirectoryUrl: URL) -> [PackageSource] {
    var packages: [PackageSource] = []
    
    do {
      // Get the URLs for all items in the directory that are not hidden
      let directoryContents = try FileManager.default.contentsOfDirectory(
        at: keyboardDirectoryUrl,
        includingPropertiesForKeys: nil,
        options: [.skipsHiddenFiles]
      )
      
      for itemUrl in directoryContents {
        // if the item is a directory, then attempt to read it as a keyboard package
        if (itemUrl.hasDirectoryPath) {
          if let packageSource =  readKeyboardPackageFromDirectory(keyboardDirectoryUrl: itemUrl) {
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

  func readKeyboardPackageFromDirectory(keyboardDirectoryUrl: URL) -> PackageSource? {
    var packageSource: PackageSource?
    let lastPathComponent = keyboardDirectoryUrl.lastPathComponent
    let kmpFileUrl = keyboardDirectoryUrl.appendingPathComponent(packageFileName)
    
    if FileManager.default.fileExists(atPath: kmpFileUrl.path) {
      if let source = readKeyboardPackage(kmpFileUrl) {
        packageSource = source
        // save the keyboard directory and path of the kmp.json for this keyboard
        packageSource?.directoryUrl = keyboardDirectoryUrl
        packageSource?.jsonFileUrl = kmpFileUrl
      }
    } else {
      print(" ** \(lastPathComponent) DOES NOT contain the file 'kmp.json')")
    }
    
    return packageSource
  }
 
  public func readKeyboardPackage(_ kmpFileUrl: URL) -> PackageSource? {
    var packageSource: PackageSource?
    do {
      let jsonData = try Data(contentsOf: kmpFileUrl, options: .mappedIfSafe)
      let source: PackageSource = try! JSONDecoder().decode(PackageSource.self, from: jsonData)
      
      print("package name: \(source.packageName)")
      packageSource = source
    } catch {
//      print("package name: \(source.packageName)")
    }
    return packageSource
  }
}
