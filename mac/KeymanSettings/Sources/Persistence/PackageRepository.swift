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

public class PackageRepository {
  fileprivate let packageFileName = "kmp.json"
  fileprivate let pathUtil: KeymanPaths
  
  public init() {
    self.pathUtil = KeymanPaths()
  }
  
  /**
   * Creates the directory tree where keyboards are stored under the standard 'Group Containers' directory
   */
  func createDataDirectories(pathUtil: KeymanPaths) {
    if let keyboardDirectory = pathUtil.keyman18KeyboardsDirectory {
      
      do {
        // create the directory if it doesn't already exist
        if !FileManager.default.fileExists(atPath: keyboardDirectory.path) {
          try FileManager.default.createDirectory(at: keyboardDirectory, withIntermediateDirectories: true, attributes: nil)
          //          ConfigLogger.shared.testLogger.debug("Created directory: \(keyboardDirectory.path)")
          print("Created directory: \(keyboardDirectory.path)")
        } else {
//          ConfigLogger.shared.testLogger.debug("Directory already exists: \(keyboardDirectory.path)")
          print("Directory already exists: \(keyboardDirectory.path)")
        }
      } catch {
//        ConfigLogger.shared.testLogger.error("Error creating directory: \(error.localizedDescription)")
        print("Error creating directory: \(error.localizedDescription)")
      }
    } else {
//      ConfigLogger.shared.testLogger.error("Unable to create directory, Group Container URL not found.")
      print("Unable to create directory, Group Container URL not found.")
    }
  }
  
  public func writeSomethingToContainer() {
    if let keyboardDirectory = self.pathUtil.keyman18KeyboardsDirectory {
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
  
  public func readKeyboardPackageSource() -> [PackageSource] {
    var packages: [PackageSource] = []
    
    if let keyboardDirectory = self.pathUtil.keyman18KeyboardsDirectory {
      do {
        // Get the URLs for all items in the directory that are not hidden
        let directoryContents = try FileManager.default.contentsOfDirectory(
          at: keyboardDirectory,
          includingPropertiesForKeys: nil,
          options: [.skipsHiddenFiles]
        )
        
        for itemUrl in directoryContents {
          // if the item is a directory, then attempt to read it as a keyboard
          if (itemUrl.hasDirectoryPath) {
            if let packageSource =  readKeyboardPackageFromDirectory(keyboardDirectoryUrl: itemUrl) {
              packages.append(packageSource)
            }
          }
        }
      } catch {
        print("Failed to read directory: \(error.localizedDescription)")
      }    } else {
        print("App Group container URL not found. Check your entitlements and provisioning profiles.")
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
//      ConfigLogger.shared.testLogger.debug(" ** \(lastPathComponent) DOES NOT contain the file 'kmp.json')")
      print(" ** \(lastPathComponent) DOES NOT contain the file 'kmp.json')")
    }
    
    return packageSource
  }
  
  public func readKeyboardPackage(_ kmpFileUrl: URL) -> PackageSource? {
    var packageSource: PackageSource?
    do {
      let jsonData = try Data(contentsOf: kmpFileUrl, options: .mappedIfSafe)
      let source: PackageSource = try! JSONDecoder().decode(PackageSource.self, from: jsonData)
      
//      ConfigLogger.shared.testLogger.debug("package name: \(source.packageName)")
      print("package name: \(source.packageName)")
      packageSource = source
    } catch {
//      ConfigLogger.shared.testLogger.debug("package name: \(source.packageName)")
//      print("package name: \(source.packageName)")
    }
    return packageSource
  }
}
