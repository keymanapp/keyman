/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * KeymanPaths provides the file paths of the Keyman input method and
 * all Keyman keyboard documents. This is used for installing and removing
 * the input method and keyboards.
 *
 * Obsolete locations are also included and used for migrating data and
 * updating settings.
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
 *      keyman17KeyboardsDirectory: '~/Documents/Keyman-Keyboards'
 * For Keyman version 18:
 *    supportDirectory: '~/Library/Application Support'
 *      supportKeymanDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman'
 *        keyman18KeyboardsDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman/Keyman-Keyboards'
 * For Keyman version 19 (and later):
 *    containerDirectory: '~/Library/Group Containers'
 *      containerKeymanDirectory: '~/Library/Group Containers/3YE4W86L3G.com.keyman'
 *        groupKeymanSupportDirectory: '~/Library/Group Containers/3YE4W86L3G.com.keyman/Library/Application Support'
 *          keyman19KeyboardsDirectory: '~/Library/Group Containers/3YE4W86L3G.com.keyman/Library/Application Support/Keyman-Keyboards'
*/

public struct KeymanPaths {
  static let keymanBundleId = "keyman.inputmethod.Keyman"
  static let configBundleId = "com.keyman.Config"
  static let groupId = "3YE4W86L3G.com.keyman"

  static private let keyboardsDirectoryName = "Keyman-Keyboards"
  static private let keymanSubdirectoryName = "keyman.inputmethod.Keyman"
  
  static private let containerPreferencesPartialPath = "Library/Preferences"
  static private let containerKeyboardsPartialPath = "Library/Application Support/Keyman-Keyboards"
  
  // keyman 17 and earlier
  let keyman17DocumentsDirectory: URL?
  let keyman17KeyboardsDirectory: URL?
  
  // keyman 18
  let keyman18SupportDirectory: URL?
  let keyman18DataDirectory: URL?
  let keyman18KeyboardsDirectory: URL?

  // current directories for keyman 19
  let keyman19KeyboardsDirectory: URL?
  let keyman19DataDirectory: URL?
  let keyman19ContainerDirectory: URL?
  let keyman19PreferencesDirectory: URL?

  public init() {
    let documentsDir = KeymanPaths.buildDocumentsUrl()
    self.keyman17DocumentsDirectory = documentsDir
    
    self.keyman17KeyboardsDirectory = KeymanPaths.buildKeyman17KeyboardsUrl(documents: self.keyman17DocumentsDirectory)
    
    let supportDir = KeymanPaths.buildSupportDirectory()
    self.keyman18SupportDirectory = supportDir
    
    let keyman18DataDir = KeymanPaths.buildKeyman18DataDirectory(support: supportDir)
    self.keyman18DataDirectory = keyman18DataDir
    self.keyman18KeyboardsDirectory = KeymanPaths.buildKeyman18KeyboardsUrl(data: keyman18DataDir)
    
    let containerDir = KeymanPaths.buildContainerUrl()
    self.keyman19ContainerDirectory = containerDir
    
    self.keyman19DataDirectory = containerDir

    self.keyman19PreferencesDirectory = KeymanPaths.buildContainerPreferencesUrl(container: containerDir)
    
    
    let keyman19KeyboardsDir = KeymanPaths.buildKeyman19KeyboardsUrl(container: containerDir)
    self.keyman19KeyboardsDirectory = keyman19KeyboardsDir
    
    //self.logPaths()
  }
  
  /*
  fileprivate func logPaths() {
    ConfigLogger.shared.testLogger.debug("documents: \(self.keyman17DocumentsDirectory!.absoluteString)")
    ConfigLogger.shared.testLogger.debug("keyman 17 keyboards: \(self.keyman17KeyboardsDirectory!.absoluteString)")
    
    ConfigLogger.shared.testLogger.debug("support directory: \(self.keyman18SupportDirectory!.absoluteString)")
    ConfigLogger.shared.testLogger.debug("support keyman directory: \(self.keyman18DataDirectory!.absoluteString)")
    ConfigLogger.shared.testLogger.debug("keyman 18 keyboards: \(self.keyman18KeyboardsDirectory!.absoluteString)")
    
    ConfigLogger.shared.testLogger.debug("container: \(self.keyman19ContainerDirectory!.absoluteString)")
    ConfigLogger.shared.testLogger.debug("preferences: \(self.keyman19PreferencesDirectory!.absoluteString)")
    ConfigLogger.shared.testLogger.debug("keyman 19 keyboards: \(self.keyman19KeyboardsDirectory!.absoluteString)")
  }
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
      
      inputMethodUrl = inputMethodDirectoryUrl.appendingPathComponent(fileName)
      return inputMethodUrl
    } catch {
//      ConfigLogger.shared.testLogger.debug("\(error)")
      print("\(error)")
      return nil
    }
  }

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
//      ConfigLogger.shared.testLogger.error("\(error)")
      print("\(error)")
     return nil
    }
  }
  
  private static func buildKeyman17KeyboardsUrl(documents: URL?) -> URL? {
    if let keyman17KeyboardsDirectory = documents?.appendingPathComponent(keyboardsDirectoryName, isDirectory: true) {
      return keyman17KeyboardsDirectory
    } else {
//      ConfigLogger.shared.testLogger.error("could not build keyman17 keyboards directory")
      print("could not build keyman17 keyboards directory")
      return nil
    }
  }

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
//      ConfigLogger.shared.testLogger.debug("\(error)")
      print("\(error)")
      return nil
    }
  }
  
  private static func buildKeyman18DataDirectory(support: URL?) -> URL? {
    if let supportUrl = support {
      return supportUrl.appendingPathComponent(KeymanPaths.keymanSubdirectoryName)
    } else {
      return nil
    }
  }

  private static func buildKeyman18KeyboardsUrl(data: URL?) -> URL? {
    if let keymanDataUrl = data {
      return keymanDataUrl.appendingPathComponent(KeymanPaths.keyboardsDirectoryName)
    } else {
      return nil
    }
  }

  private static func buildContainerUrl() -> URL? {
    return FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: KeymanPaths.groupId)
  }

  private static func buildContainerPreferencesUrl(container: URL?) -> URL? {
    if let containerUrl = container {
      return containerUrl.appendingPathComponent(containerPreferencesPartialPath)
    } else {
      return nil
    }
  }

  private static func buildKeyman19KeyboardsUrl(container: URL?) -> URL? {
    if let containerUrl = container {
      return containerUrl.appendingPathComponent(KeymanPaths.containerKeyboardsPartialPath)
    } else {
      return nil
    }
  }

  // TODO: not used, remove?
  fileprivate func checkContainerUrl() -> Bool {
    var containerValid = false
    let sharedFileManager = FileManager.default
    
    /* a URL of the expected form is always returned, even if the app group is invalid, so verify access before using" */
    
    if let containerUrl = sharedFileManager.containerURL(forSecurityApplicationGroupIdentifier: KeymanPaths.groupId) {
      containerValid = true
//      ConfigLogger.shared.testLogger.debug("containerUrl = \(containerUrl)")
      print("containerUrl = \(containerUrl)")
    }
    
    return containerValid
  }
}
