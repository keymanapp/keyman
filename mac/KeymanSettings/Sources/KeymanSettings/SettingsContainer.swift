/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-04-02
 *
 * Class that exposes all settings information to the Keyman Configuration app
 * Provides a place for the config app can bind directly to the settings
 * and update when changes are made
 *
 */

import Foundation
import Combine

public enum SettingsError: Error {
    case unknownPackage
}

public class SettingsContainer : ObservableObject {
  // packages are loaded from disk, each package may contain one or more keyboard
  @Published public var installedPackages: [KeymanPackage]
  
  fileprivate let packageRepository: PackageRepository
  fileprivate let settingsRepository: SettingsRepository
  
  // the selected keyboard is stored in the UserDefaults
  fileprivate var currentKeyboard: String
#warning("does the config app need to be aware of this?")

  public init() {
    self.packageRepository = PackageRepository()
    
    // create the settings repository, gaining access to the app group UserDefaults
    do {
      try self.settingsRepository = SettingsRepository(suiteName: KeymanPaths.groupId)
      print("Found group container")
    } catch UserDefaultsError.unknownSuite {
      fatalError("Group container not found.")
    } catch {
      fatalError("Unable to access settings in group container.")
    }
    
    self.currentKeyboard = self.settingsRepository.readSelectedKeyboard()
    
    
    // first load all the installed packages from disk
    self.installedPackages = []
    if let persistedPackages = self.loadPackages() {
      self.installedPackages = persistedPackages
    }
    
    // next, apply the settings to the packages
    // this mainly consists of marking them as enabled or not
    self.applySettingsToInstalledPackages()
  }
  
#warning("delete test code")
  public func debug() {
   self.installedPackages .forEach { package in
     package.keyboards.forEach { keyboard in
       print("\(keyboard.keyboardId) enabled: \(keyboard.enabled)")
      }
    }
  }

  /**
   * for debugging: prints UserDefaults values
   */
  public func logSettings() {
    self.settingsRepository.logSettings()
  }

  public func clearSettings() {
    self.settingsRepository.clearSettings()
  }

  public func findPackage(packageId: UUID) throws -> KeymanPackage {
    guard let package = self.installedPackages.first(where: { $0.id == packageId }) else {
      print ("Error: could not find package with ID: \(packageId)")
      throw SettingsError.unknownPackage
    }
    
    return package
  }
  
  public func removePackage(at index: Int) {
    let package = self.installedPackages[index]
    
    // will removing this package cause the removal of any enabled keyboards?
    let removingEnabledKeyboards = !package.getEnabledKeyboardsSettingsKeys().isEmpty

    // delete package from disk
    self.packageRepository.deletePackage(package: package)
    
    // remove package from installed packages list
    _ = self.installedPackages.remove(at: index)
    
    // if we removed any enabled keyboards, then update settings
    if removingEnabledKeyboards {
      self.persistKeyboardState()
    }
  }
  /**
   * returns true if the keyboard is enabled
   * when enabled, the keyboard appears in the Keyman sub menu in the mac
   */
  public func isKeyboardEnabled(packageId: UUID, keyboardId: String) -> Bool {
    var enabled = false;

    if let package = try? self.findPackage(packageId: packageId) {
      enabled = package.isKeyboardEnabled(keyboardId: keyboardId)
      print ("isEnabled for \(keyboardId) returning with \(enabled)")
    } else {
      print ("Could not read keyboard state for package: \(packageId) and keyboard: \(keyboardId)")
    }
    
    return enabled
  }
    
  /**
   * enable or disable the keyboard
   */
  public func setKeyboardEnabled(packageId: UUID, keyboardId: String, enabled: Bool) {
    if let package = try? self.findPackage(packageId: packageId) {
      // update state of Keyboard
      print ("setKeyboardEnabled for \(keyboardId) setting to \(enabled)")
      package.enableKeyboard(keyboardId: keyboardId, enabled: enabled)
    } else {
      print ("Could not read keyboard state for package: \(packageId) and keyboard: \(keyboardId)")
    }
    
    // update persisted state in UserDefaults enabledKeyboards array
    self.persistKeyboardState()
  }

  /**
   * persist the keyboard state in the settings (UserDefaults)
   */
  func persistKeyboardState() {
    let enabledKeyboards = self.getAllEnabledKeyboardSettingsKeys()
    self.settingsRepository.writeEnabledKeyboards(enabledKeyboardsArray: Array(enabledKeyboards))
  }
  
  /**
   *  read the Keyman packages from the group container directory and store in the keyboardPackages array
   */
  func loadPackages() -> [KeymanPackage]? {
    var packagesArray = nil as [KeymanPackage]?

    // load keyboards from disk
    if (self.packageRepository.keyman19SharedDataDirectoryExists()) {      
      packagesArray = self.packageRepository.loadPackages()
    } else {
      self.packageRepository.createKeyman19SharedDataDirectories()
    }
    
    return packagesArray
  }
  
  /**
   *  returns set containing the keyboards settings keys for all installed keyboards
   */
  func getAllKeyboardSettingsKeys() -> Set<String> {
    var settingsKeys = Set<String>()
    
    // loop through all the installed packages and for each of the package's keyboards,
    // insert the settings key for the keyboard
    self.installedPackages.forEach { $0.keyboards.forEach
      {settingsKeys.insert($0.keyboardSettingsKey)}
    }

    return settingsKeys
  }
  
  /**
   *  returns set containing the keyboards settings keys for all installed keyboards which are enabled
   */
  func getAllEnabledKeyboardSettingsKeys() -> Set<String> {
    var settingsKeys = Set<String>()
    
    // loop through all the installed packages and for each of the package's keyboards,
    // insert the settings key for every enabled keyboard
    self.installedPackages.forEach { $0.keyboards.forEach {
        if ($0.enabled) {
          settingsKeys.insert($0.keyboardSettingsKey)
        }
      }
    }

    return settingsKeys
  }
  
  /**
   *  remove any settings (UserDefaults) for which we have no installed package
   */
  func validateSettings() {
    let installedKeyboardKeys = self.getAllKeyboardSettingsKeys()
    let enabledKeyboardKeys = self.settingsRepository.readEnabledKeyboards()
    
    if (enabledKeyboardKeys.isSubset(of: installedKeyboardKeys)) {
      print("only installed keyboards are listed as enabled: no need to synchronize")
    } else {
      print("enabled keyboards list contains uninstalled keyboards: synchronize enabled keyboards list")
      let installedEnabledKeyboardKeys = enabledKeyboardKeys.intersection(installedKeyboardKeys)
      self.settingsRepository.writeEnabledKeyboards(enabledKeyboardsArray: Array(installedEnabledKeyboardKeys))
    }
  }
  
  /**
   *  apply the state from the current settings to the installed packages
   */
  func applySettingsToInstalledPackages() {
    self.validateSettings()
    
    let enabledKeyboards = self.settingsRepository.readEnabledKeyboards()

    // set enabled flag if the keyboard is contained in the set of enabledKeyboards
    self.installedPackages.forEach { $0.keyboards.forEach
      {$0.enabled = enabledKeyboards.contains($0.keyboardSettingsKey)}
    }
  }
}
