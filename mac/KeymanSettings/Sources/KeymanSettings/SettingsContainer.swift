/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-04-02
 *
 * Class that exposes all settings information to the Config app
 * Provides a place for the Config app can bind directly to the settings
 * and update when changes are made
 *
 */

import Foundation
import Combine

public class SettingsContainer : ObservableObject {
  // keyboard packages are defined on disk
  @Published public var installedKeyboardPackages: [KeymanPackage]
  
  fileprivate let packageRepository: PackageRepository
  fileprivate let settingsRepository: SettingsRepository
  
  // active keyboards and selected keyboard are stored in the UserDefaults
  fileprivate var activeKeyboards: Set<String>
  fileprivate var currentKeyboard: String
  
  public init() {
    self.packageRepository = PackageRepository()
    
    do {
      try self.settingsRepository = SettingsRepository(suiteName: KeymanPaths.groupId)
      print("Found group container")
    } catch UserDefaultsError.unknownSuite {
      fatalError("Group container not found.")
    } catch {
      fatalError("Unable to access settings in group container.")
    }
    
    self.activeKeyboards = self.settingsRepository.readActiveKeyboards()
    self.currentKeyboard = self.settingsRepository.readSelectedKeyboard()
    
    self.installedKeyboardPackages = []
    self.loadPackages()
    self.synchronizeSettings()
    self.applySettings()
  }
  
  // TODO: delete test code
  public func debug() {
   self.installedKeyboardPackages .forEach { package in
     package.keyboards.forEach { keyboard in
       print("\(keyboard.keyboardId) enabled: \(keyboard.enabled)")
      }
    }
  }

  public func logSettings() {
    self.settingsRepository.logSettings()
  }

  public func clearSettings() {
    self.settingsRepository.clearSettings()
  }

  // TODO: extract method for keyboard search
  // TODO: throw error for keyboard not found or log and fail silently (should never happen)
  public func isKeyboardEnabled(packageId: UUID, keyboardId: String) -> Bool {
    var enabled = false;
    
    guard let package = self.installedKeyboardPackages.first(where: { $0.id == packageId }) else {
      print ("ERROR: setKeyboardEnabled could not find package with ID: \(packageId)")
      return enabled
    }
    
    enabled = package.isKeyboardEnabled(keyboardId: keyboardId)
    print ("isEnabled for \(keyboardId) returning with \(enabled)")

    return enabled
  }
    
  public func setKeyboardEnabled(packageId: UUID, keyboardId: String, enabled: Bool) {
    let package = self.installedKeyboardPackages.first(where: { $0.id == packageId })
    if (package == nil) {
      print ("ERROR: setKeyboardEnabled could not find package with ID: \(packageId)")
    } else {
      package!.enableKeyboard(keyboardId: keyboardId, enabled: enabled)
      print ("setKeyboardEnabled for \(keyboardId) setting to \(enabled)")
    }
  }
    
  /**
   *  read the Keyman packages from the group container directory and store in the keyboardPackages array
   */
  func loadPackages() {
    // TODO: create keyboards directory if it doesn't exist
    //    if let keyboardsUrl = self.pathUtil.keymanKeyboardsDirectory {
    //
    //      if FileManager.default.fileExists(atPath: keyboardsUrl.path) {
    //        print("directory exists: \(keyboardsUrl.absoluteString)")
    //      } else {
    //        print("non-existent directory: \(keyboardsUrl.absoluteString)")
    //      }
    //    }

    // load keyboards from disk
    if (self.packageRepository.keyman19SharedDataDirectoryExists()) {
      // TODO: remove test code
      self.packageRepository.writeSomethingToContainer()
      
      self.installedKeyboardPackages = self.packageRepository.loadPackages()
    } else {
      self.packageRepository.createKeyman19SharedDataDirectories()
    }
  }
  
  /**
   *  returns set containing the keyboards settings keys for the keyboards of every installed package
   */
  func getAllKeyboardSettingsKeys() -> Set<String> {
    var settingsKeys = Set<String>()
    
    // loop through all the installed packages and for each of the package's keyboards,
    // insert the settings key for the keyboard
    self.installedKeyboardPackages.forEach { $0.keyboards.forEach
      {settingsKeys.insert($0.keyboardSettingsKey)}
    }

    return settingsKeys
  }
  
  /**
   *  remove any settings (UserDefaults) for which we have no installed package
   */
  func synchronizeSettings() {
    let installedPackageKeys = self.getAllKeyboardSettingsKeys()
    let activeKeyboardKeys = self.settingsRepository.readActiveKeyboards()
    let commonKeyboardKeys = installedPackageKeys.intersection(activeKeyboardKeys)

    // TODO: replace the active keyboards list with the intersection of the current
    // active keyboards list and the installed packages list
  }
  
  /**
   *  apply the current settings to the installed packages
   */
  func applySettings() {
    
  }
  
}
