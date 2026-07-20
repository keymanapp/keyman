/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * KeymanPaths provides the file paths of the Keyman input method and
 * all Keyman package data. This is used for installing and removing
 * the input method and packages.
 */

import Foundation

/**
 * Three directory trees are represented by the following properties, one in active use
 * and two that are obsolete.
 *
 * Each of the directory properties defined ending with the word 'Directory' represents a full path .
 * Variables that represent only a single directory end with 'DirectoryName' rather than 'Directory'
 * For Keyman versions 17 and earlier, the directory hierarchy is as follows:
 *    documentsDirectory: '~/Documents'
 *      keyman17PackagesDirectory: '~/Documents/Keyman-Keyboards'
 * For Keyman version 18:
 *    supportDirectory: '~/Library/Application Support'
 *      supportKeymanDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman'
 *        keyman18PackagesDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman/Keyman-Keyboards'
 * For Keyman version 19 (and later):
 *    containerDirectory: '~/Library/Group Containers'
 *      containerKeymanDirectory: '~/Library/Group Containers/group.com.keyman'
 *        groupKeymanSupportDirectory: '~/Library/Group Containers/group.com.keyman/Library/Application Support'
 *          keyman19PackagesDirectory: '~/Library/Group Containers/group.com.keyman/Library/Application Support/Keyman-Packages'
 */

public enum KeymanPathError: Error {
  case groupContainerNotFound
}

public struct KeymanPaths {
  // MAC-CONFIG-TODO: move to input method util?
  static public let keymanBundleId = "keyman.inputmethod.Keyman"
  static public let configBundleId = "com.keyman.config"
  static public let groupId = "group.com.keyman"
  
    // keyman file extensions
  static public let keymanPackageFileExtension: String = "kmp"

  static private let preKeyman19PackagesDirectoryName = "Keyman-Keyboards"
  static private let keymanSubdirectoryName = keymanBundleId
  
  // keyman 19 directory names
  static private let containerPreferencesPartialPath = "Library/Preferences"
  static private let containerPackagesPartialPath = "Library/Application Support/Keyman-Packages"
  static private let containerTempPartialPath = "Library/Application Support/temp"

  // keyman 17 and earlier
  let keyman17DocumentsDirectory: URL?
  let keyman17PackagesDirectory: URL?
  
  // keyman 18
  let keyman18SupportDirectory: URL?
  let keyman18DataDirectory: URL?
  let keyman18PackagesDirectory: URL?
  
  // current directories for keyman 19
  let keyman19ContainerDirectory: URL
  let keyman19PackagesDirectory: URL
  let keyman19TempDirectory: URL
  let keyman19PreferencesDirectory: URL
  
  /**
   * Build the paths to the Keyman data locations
   * throws `KeymanPathError` if the group container directory cannot be located
   * The app cannot function without access to where the Keyman packages are stored.
   */
  public init() throws {
    let documentsDir = KeymanPaths.buildDocumentsUrl()
    self.keyman17DocumentsDirectory = documentsDir
    
    self.keyman17PackagesDirectory = KeymanPaths.buildKeyman17PackagesUrl(documents: self.keyman17DocumentsDirectory)
    
    let supportDir = KeymanPaths.buildSupportDirectory()
    self.keyman18SupportDirectory = supportDir
    
    let keyman18DataDir = KeymanPaths.buildKeyman18DataDirectory(support: supportDir)
    self.keyman18DataDirectory = keyman18DataDir
    self.keyman18PackagesDirectory = KeymanPaths.buildKeyman18PackagesUrl(data: keyman18DataDir)
    
    guard let containerDir = KeymanPaths.buildContainerUrl() else { throw KeymanPathError.groupContainerNotFound }
    self.keyman19ContainerDirectory = containerDir
    self.keyman19PreferencesDirectory = KeymanPaths.buildContainerPreferencesUrl(container: containerDir)
    
    
    self.keyman19PackagesDirectory = KeymanPaths.buildKeyman19PackagesUrl(container: containerDir)
    self.keyman19TempDirectory = KeymanPaths.buildKeyman19TempUrl(container: containerDir)
    //self.logPaths()
  }
  
  /*
   fileprivate func logPaths() {
   ConfigLogger.shared.testLogger.debug("documents: \(self.keyman17DocumentsDirectory!.absoluteString)")
   ConfigLogger.shared.testLogger.debug("keyman 17 packages: \(self.keyman17PackagesDirectory!.absoluteString)")
   
   ConfigLogger.shared.testLogger.debug("support directory: \(self.keyman18SupportDirectory!.absoluteString)")
   ConfigLogger.shared.testLogger.debug("support keyman directory: \(self.keyman18DataDirectory!.absoluteString)")
   ConfigLogger.shared.testLogger.debug("keyman 18 packages: \(self.keyman18PackagesDirectory!.absoluteString)")
   
   ConfigLogger.shared.testLogger.debug("container: \(self.keyman19ContainerDirectory!.absoluteString)")
   ConfigLogger.shared.testLogger.debug("preferences: \(self.keyman19PreferencesDirectory!.absoluteString)")
   ConfigLogger.shared.testLogger.debug("keyman 19 packages: \(self.keyman19PackagesDirectory!.absoluteString)")
   }
   */
  
  /**
   * build the URL to specified file in the Input Methods directory
   */
  public func buildInputMethodPathUrl(fileName:String) -> URL? {
    let inputMethodUrl:URL
    
    do {
      let inputMethodDirectoryUrl = try FileManager.default.url(
        for: .inputMethodsDirectory,
        in: .userDomainMask,
        appropriateFor: nil,
        create: true
      )
      
      inputMethodUrl = inputMethodDirectoryUrl.appendingPathComponent(fileName, isDirectory: false)
      return inputMethodUrl
    } catch {
      //      ConfigLogger.shared.testLogger.debug("\(error)")
      print("\(error)")
      return nil
    }
  }
  
  public func buildInputMethodExecutableUrl(fileName:String) -> URL? {
    if let inputMethodUrl = self.buildInputMethodPathUrl(fileName: fileName) {
      let executableName = inputMethodUrl.deletingPathExtension().lastPathComponent
      return inputMethodUrl.appendingPathComponent("Contents/MacOS/\(executableName)")
    } else {
      return nil
    }
  }

  /**
   * build the URL to the user's Documents directory
   */
  static func buildDocumentsUrl() -> URL? {
    var documentsDirectoryUrl:URL
    
    do {
      documentsDirectoryUrl = try FileManager.default.url(
        for: .documentDirectory,
        in: .userDomainMask,
        appropriateFor: nil,
        create: true
      )
      return documentsDirectoryUrl
    } catch {
      print("\(error)")
      return nil
    }
  }
  
  /**
   * build the URL to the packages directory for Keyman 17, inside the user's Documents directory
   */
  private static func buildKeyman17PackagesUrl(documents: URL?) -> URL? {
    if let keyman17PackagesDirectory = documents?.appendingPathComponent(preKeyman19PackagesDirectoryName, isDirectory: true) {
      return keyman17PackagesDirectory
    } else {
      print("could not build keyman17 packages directory")
      return nil
    }
  }
  
  /**
   * build the URL to the application support directory for the Keyman input method
   */
  private static func buildSupportDirectory() -> URL? {
    let supportDirectoryUrl:URL
    
    do {
      supportDirectoryUrl = try FileManager.default.url(
        for: .applicationSupportDirectory,
        in: .userDomainMask,
        appropriateFor: nil,
        create: true
      )
      
      return supportDirectoryUrl
    } catch {
      print("\(error)")
      return nil
    }
  }
  
  /**
   * build the URL to the data directory for Keyman 18, inside the application support directory for the Keyman input method
   */
  private static func buildKeyman18DataDirectory(support: URL?) -> URL? {
    if let supportUrl = support {
      return supportUrl.appendingPathComponent(KeymanPaths.keymanSubdirectoryName, isDirectory: true)
    } else {
      return nil
    }
  }
  
  /**
   * build the URL to the packages directory for Keyman 18, inside the application support directory for the Keyman input method
   */
  private static func buildKeyman18PackagesUrl(data: URL?) -> URL? {
    if let keymanDataUrl = data {
      return keymanDataUrl.appendingPathComponent(KeymanPaths.preKeyman19PackagesDirectoryName, isDirectory: true)
    } else {
      return nil
    }
  }
  
  /**
   * build the URL to the app group container
   */
  private static func buildContainerUrl() -> URL? {
    return FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: KeymanPaths.groupId)
  }
  
  /**
   * build the URL to the preference directory inside the app group container
   */
  private static func buildContainerPreferencesUrl(container: URL) -> URL {
    return container.appendingPathComponent(containerPreferencesPartialPath, isDirectory: true)
  }
  
  /**
   * build the URL to the packages directory inside the app group container directory
   */
  private static func buildKeyman19PackagesUrl(container: URL) -> URL {
    return container.appendingPathComponent(KeymanPaths.containerPackagesPartialPath, isDirectory: true)
  }
  
  /**
   * build the URL to the temp directory inside the app group container directory
   */
  private static func buildKeyman19TempUrl(container: URL) -> URL {
    return container.appendingPathComponent(KeymanPaths.containerTempPartialPath, isDirectory: true)
  }

  // MAC-CONFIG-TODO: remove
  fileprivate func checkContainerUrl() -> Bool {
    var containerValid = false
    let sharedFileManager = FileManager.default
    
    /* a URL of the expected form is always returned, even if the app group is invalid, so verify access before using" */
    
    if let containerUrl = sharedFileManager.containerURL(forSecurityApplicationGroupIdentifier: KeymanPaths.groupId) {
      containerValid = true
      print("containerUrl = \(containerUrl)")
    }
    
    return containerValid
  }
}
