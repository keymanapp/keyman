/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-04-02
 *
 * Class that exposes all settings information to the Keyman Configuration app
 * Provides a place for the config app can bind directly to the settings
 * and update when changes are made
 *
 * The Settings consist of two types of data:
 * - Keyman packages that have been installed on disk in the Group Containers directory
 * - Some lightwieght settings that are stored in the macOS UserDefaults Database
 *
 * Both the packages and the defaults are in a shared location that can be accessed by
 * the Keyman config app and the Keyman input method.
 *
 * This class accesses the two types of data through PackageRepository and DefaultsRepository
 * and publishes the combined data to the UI through the `installedPackages` array.
 */

import Foundation
import Combine

public enum SettingsError: Error {
    case unknownPackage
}

public class SettingsContainer : ObservableObject {
  // packages are loaded from disk, each package may contain one or more keyboard
  @Published public var installedPackages: [KeymanPackage]
  
  fileprivate let packageRepository: PackageRepo
  fileprivate let defaultsRepository: DefaultsRepo
  
  // the selected keyboard is stored in the UserDefaults
  fileprivate var selectedKeyboard: String
#warning("does the config app need to be aware of this?")

  public init() {
    self.packageRepository = PackageRepository()
    
    // create the settings repository, gaining access to the app group UserDefaults
    do {
      try self.defaultsRepository = DefaultsRepository(suiteName: KeymanPaths.groupId)
      print("Found group container")
    } catch UserDefaultsError.unknownSuite {
      fatalError("Group container not found.")
    } catch {
      fatalError("Unable to access settings in group container.")
    }
    
    self.selectedKeyboard = self.defaultsRepository.readSelectedKeyboard()
    
    
    // first load all the installed packages from disk
    self.installedPackages = []
    self.loadPackages()
    
    // next, apply the settings to the packages
    // this mainly consists of marking them as enabled or not
    self.applySettingsToInstalledPackages()
  }

  public init(defaultsRepo: DefaultsRepo, packageRepo: PackageRepo) {
    self.defaultsRepository = defaultsRepo
    self.packageRepository = packageRepo
    self.selectedKeyboard = self.defaultsRepository.readSelectedKeyboard()

    self.installedPackages = []
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
    self.defaultsRepository.logDefaults()
  }

  public func clearSettings() {
    self.defaultsRepository.clearDefaults()
  }

  public func findPackage(packageId: UUID) -> KeymanPackage? {
    guard let package = self.installedPackages.first(where: { $0.id == packageId }) else {
      print ("Error: could not find package with ID: \(packageId)")
      return nil
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
  public func isKeyboardEnabled(packageId: UUID, keyboardKey: String) -> Bool {
    guard let package = self.findPackage(packageId: packageId) else {
      print ("Could not read keyboard state for package: \(packageId) and keyboard: \(keyboardKey)")
      return false
    }

    let enabled = package.isKeyboardEnabled(keyboardKey: keyboardKey)
    print ("isEnabled for \(keyboardKey) returning with \(enabled)")
    
    return enabled
  }
    
  /**
   * enable or disable the keyboard
   */
  public func setKeyboardEnabled(packageId: UUID, keyboardKey: String, enabled: Bool) {
    guard let package = self.findPackage(packageId: packageId) else {
      print ("Could not read keyboard state for package: \(packageId) and keyboard: \(keyboardKey)")
      return
    }

    print ("setKeyboardEnabled for \(keyboardKey) setting to \(enabled)")
    package.enableKeyboard(keyboardKey: keyboardKey, enabled: enabled)
    
    // update persisted state in UserDefaults enabledKeyboards array
    self.persistKeyboardState()
  }

  /**
   * persist the keyboard state in the settings (UserDefaults)
   */
  func persistKeyboardState() {
    let enabledKeyboards = self.getAllEnabledKeyboardKeys()
    self.defaultsRepository.writeEnabledKeyboards(enabledKeyboardsArray: Array(enabledKeyboards))
  }
  
  /**
   *  read the Keyman packages from the group container directory and store in the keyboardPackages array
   */
  func loadPackages() {
   var packagesArray = nil as [KeymanPackage]?

    // read keyboards from disk
    if (self.packageRepository.keyman19SharedDataDirectoryExists()) {      
      packagesArray = self.packageRepository.loadPackages()
    } else {
      self.packageRepository.createKeyman19SharedDataDirectories()
    }
    
    if let persistedPackages = packagesArray {
      self.installedPackages = persistedPackages
    }
  }
  
  /**
   *  returns set containing the keyboards settings keys for all installed keyboards
   */
  func getAllKeyboardKeys() -> Set<String> {
    var settingsKeys = Set<String>()
    
    // loop through all the installed packages and for each of the package's keyboards,
    // insert the settings key for the keyboard
    self.installedPackages.forEach { $0.keyboards.forEach
      {settingsKeys.insert($0.keyboardKey)}
    }

    return settingsKeys
  }
  
  /**
   *  returns set containing the keyboards settings keys for all installed keyboards which are enabled
   */
  func getAllEnabledKeyboardKeys() -> Set<String> {
    var settingsKeys = Set<String>()
    
    // loop through all the installed packages and for each of the package's keyboards,
    // insert the settings key for every enabled keyboard
    self.installedPackages.forEach { $0.keyboards.forEach {
        if ($0.enabled) {
          settingsKeys.insert($0.keyboardKey)
        }
      }
    }

    return settingsKeys
  }
  
  /**
   *  remove any settings (UserDefaults) for which we have no installed package
   */
  func validateSettings() {
    let installedKeyboardKeys = self.getAllKeyboardKeys()
    let enabledKeyboardKeys = self.defaultsRepository.readEnabledKeyboards()
    
    if (enabledKeyboardKeys.isSubset(of: installedKeyboardKeys)) {
      print("only installed keyboards are listed as enabled: no need to synchronize")
    } else {
      print("enabled keyboards list contains uninstalled keyboards: synchronize enabled keyboards list")
      let installedEnabledKeyboardKeys = enabledKeyboardKeys.intersection(installedKeyboardKeys)
      self.defaultsRepository.writeEnabledKeyboards(enabledKeyboardsArray: Array(installedEnabledKeyboardKeys))
    }
  }
  
  /**
   *  apply the state from the current settings to the installed packages
   */
  func applySettingsToInstalledPackages() {
    self.validateSettings()
    
    let enabledKeyboards = self.defaultsRepository.readEnabledKeyboards()

    // set enabled flag if the keyboard is contained in the set of enabledKeyboards
    self.installedPackages.forEach { $0.keyboards.forEach
      {
        let keyboard = $0
        print("before keyboard \(keyboard.name) has enabled status: \(keyboard.enabled)")
        $0.enabled = enabledKeyboards.contains($0.keyboardKey)
        print("after keyboard \(keyboard.name) has enabled status: \(keyboard.enabled)")
      }
    }
  }
}
