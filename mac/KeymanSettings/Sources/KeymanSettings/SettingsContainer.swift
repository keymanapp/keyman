/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-04-02
 *
 * Class that exposes all settings information to the Keyman Configuration app
 * Provides a place for the config app to bind directly to the settings
 * and update when changes are made
 *
 * The Settings consist of two types of data:
 * - Keyman packages that have been installed on disk in the Group Containers directory
 * - Some lightweight preferences that are stored in the macOS UserDefaults Database
 *
 * Both the packages and the defaults are in a shared location that can be accessed by
 * the Keyman config app and the Keyman input method.
 *
 * This class accesses the two types of data through PackageRepository and DefaultsRepository
 * and publishes the combined data to the UI through the `installedPackages` array.
 *
 * All installed packages are display in the configuration app.
 * Only installed packages that are enabled appear in the Keyman menu presented by the input method.
 */

import Foundation
import Combine
import ZIPFoundation

// distributed notifications
public extension Notification.Name {
  static let accessibilityQueryResponse = Notification.Name("com.keyman.accessibility.state")
  static let keyboardsChanged = Notification.Name("com.keyman.keyboards.changed")
}

// in-app notifications
public extension Notification.Name {
  static let newPackageInstalled = Notification.Name("com.keyman.package.installed")
  static let packageReplaced = Notification.Name("com.keyman.package.replaced")
  static let packageDowngradeRequested = Notification.Name("com.keyman.package.downgrade.requested")
}

public enum SettingsError: Error {
  case unknownPackage
}

@MainActor // run on the main actor since data is published directly to the UI
public class SettingsContainer : ObservableObject {
  // installed packages are loaded from disk, each package may contain one or more keyboard
  fileprivate var installedPackages: [KeymanPackage] {
    didSet {
      // whenever this array is modified, update the arrays that are derived from it
      self.updatePackageArrays()
    }
  }
  
  // Maintain two arrays for use by configuration views.
  // One is for packages with single keyboards and one for packages with multiple keyboards.
  // Each array is sorted alphabetically by package name.
  // These arrays are updated by a property observer on installedPackages.
  // (Consider installedPackages as the source of truth and these arrays for presentation purposes.)
  @Published public private(set) var singleKeyboardPackages: [KeymanPackage]
  @Published public private(set) var multiKeyboardPackages: [KeymanPackage]

  // when a new package is downloaded, it is tracked here
  public private(set) var packageDownload: PackageDownload? = nil
  
  fileprivate let packageRepository: PackageRepo
  fileprivate let defaultsRepository: DefaultsRepo
  
  // the selected keyboard is stored in the UserDefaults
  // not indicated in the Config app but this could change
  fileprivate var selectedKeyboard: String
  
  public init() {
    // initialize arrays before loading packages
    self.singleKeyboardPackages = []
    self.multiKeyboardPackages = []
    self.installedPackages = []

    // create the package repository, gaining access to the app group container directory
    do {
      try self.packageRepository = PackageRepository()
      print("Found documents group container")
    } catch KeymanPathError.groupContainerNotFound {
      fatalError("Document group container not found.")
    } catch {
      fatalError("Unable to access documents in group container.")
    }

    // create the settings repository, gaining access to the app group UserDefaults
    do {
      try self.defaultsRepository = DefaultsRepository(suiteName: KeymanPaths.groupId)
      print("Found defaults group container")
    } catch UserDefaultsError.unknownSuite {
      fatalError("Defaults group container not found.")
    } catch {
      fatalError("Unable to access defaults in group container: \(error.localizedDescription).")
    }

    self.selectedKeyboard = self.defaultsRepository.readSelectedKeyboard()

    // load all the installed packages from disk
    // though we are in the initializer, didset will be called to update
    // the two dependent package arrays because of the call to this helper method
    self.loadPackages()
    
    // next, apply the settings to the packages
    // this mainly consists of marking them as enabled or not
    self.applyUserDefaultsToInstalledPackages()
    
    // use NotificationCenter to receive keyboard installation notifications
    self.registerObservers()
  }
  
  /**
   * initializer only for use with unit tests, allows injection of stubs for `DefaultsRepo` and `PackageRepo`
   */
  init(defaultsRepo: DefaultsRepo, packageRepo: PackageRepo) {
    self.defaultsRepository = defaultsRepo
    self.packageRepository = packageRepo
    self.selectedKeyboard = self.defaultsRepository.readSelectedKeyboard()
    
    self.singleKeyboardPackages = []
    self.multiKeyboardPackages = []
    self.installedPackages = []
  }
  
  /**
   * register observers to handle notifications
   */
  func registerObservers() {
    // for installation of a new package
    NotificationCenter.default.addObserver(
        self, selector: #selector(newPackageInstalled(_:)),
        name: .newPackageInstalled, object: nil
    )
    
    // for replacement of an existing package
    NotificationCenter.default.addObserver(
        self, selector: #selector(existingPackageReplaced(_:)),
        name: .packageReplaced, object: nil
    )
  }
  
  /**
   * called for `newPackageInstalled` notification
   */
  @objc func newPackageInstalled(_ notification: Notification) {
    print("newPackageInstalled notification received")
    self.addInstalledPackage()
    self.packageDownload = nil
  }

  /**
   * called for `packageReplaced` notification
   */
  @objc func existingPackageReplaced(_ notification: Notification) {
    print("existingPackageReplaced notification received")
    self.replaceInstalledPackage()
    self.packageDownload = nil
  }
  
  /**
   * Whenever the installedPackages array changes, recreate the two subarrays
   */
  public func updatePackageArrays() {
    self.singleKeyboardPackages = []
    self.multiKeyboardPackages = []
    
    // divide the installedPackages array into two separate arrays based on whether they have one or more keyboards
    let partitionedPackages = self.installedPackages.reduce(into: (single: [KeymanPackage](), multiple: [KeymanPackage]())) { result, element in
      if element.keyboards.count == 1 {
        result.single.append(element)
      } else if element.keyboards.count > 1 {
        result.multiple.append(element)
      }
    }
    
    // sort subarrays alphabetically, without regard to case, using the 'packageName' property
    self.singleKeyboardPackages = partitionedPackages.single.sorted { $0.packageName.caseInsensitiveCompare($1.packageName) == .orderedAscending }
    self.multiKeyboardPackages = partitionedPackages.multiple.sorted { $0.packageName.caseInsensitiveCompare($1.packageName) == .orderedAscending }
  }

  /**
   * Called when user approves the downgrade of package
   */
  public func userConfirmedPackageDowngrade() {
    self.replaceInstalledPackage()
    self.packageDownload = nil
  }

  /**
   * Called when user chooses to cancel downgrade of package
   */
  public func userCanceledPackageDowngrade() {
    if let download = self.packageDownload {
      do {
        try download.cancelInstallation()
      } catch {
        print("downgrade cancelled but failed to cancel installation")
      }
    }
  
    self.packageDownload = nil
  }
  
  // MAC-CONFIG-TODO: delete test code
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
  public func logUserDefaults() {
    self.defaultsRepository.logDefaults()
  }
  
  /**
   * for debugging: clears all UserDefaults values
   */
  public func clearUserDefaults() {
    self.defaultsRepository.clearDefaults()
  }
  
  /**
   * check whether a download is already in progress
   */
  public func isDownloadInProgress() -> Bool {
    // MAC-CONFIG-TODO: add logic, this does not actually prevent downloads when hard-coded to true
   return false
  }

  /**
   * Called by the WebView Coordinator before initiating a package download.
   * Creates a PackageDownload instance to manage the state of the package being downloaded with the specified name.
   * Returns a URL to the temporary location where the package is to be downloaded as a .kmp file.
   */
  public func preparePackageDownload(kmpFileName: String) -> URL? {
    // package name is filename minus .kmp extension
    let packageName = kmpFileName.replacingOccurrences(of: ".kmp", with: "")
    
    let packageDownload = PackageDownload(filename: kmpFileName, packageName: packageName, packageRepo: self.packageRepository, installedPackages: self.installedPackages)

    self.packageDownload = packageDownload
    return packageDownload.temporaryKmpFileLocation
  }
  
  /**
   * Called by the WebView Coordinator after the download is complete.
   * Delegates to the PackageDownload instance to decide whether the package should be installed.
   */
  public func packageDownloadComplete(kmpFileUrl: URL) {
    print ("packageDownloadComplete \(kmpFileUrl)")

    self.packageDownload?.packageDownloadComplete(for: kmpFileUrl)
  }

  /**
   * The package is approved for installation, so add it to the package list and update the UserDefaults for enabled keyboards
   */
  func addInstalledPackage() {
    if let package = self.packageDownload?.packageToInstall {
      self.installedPackages.append(package)
      self.addEnabledKeyboards(for: package)
    }
  }
  
  /**
   * The package is approved for installation, so replace the package of the same name in the package list.
   * Also update the UserDefaults for enabled keyboards because the new package is enabled by default, and the existing may be disabled
   */
  func replaceInstalledPackage() {
    if let package = self.packageDownload?.packageToInstall {
      if let index = self.installedPackages.firstIndex(where: { $0.packageName == package.packageName }) {
        self.installedPackages[index] = package
        self.addEnabledKeyboards(for: package)
      } else {
        print("Error: package '\(package.packageName)' not found for replacement")
      }
    }
  }
  
  /**
   *  for each enabled keyboard in the package being installed, add it to the enabled keyboards set and save it in the UserDefaults
   */
  func addEnabledKeyboards(for installedPackage: KeymanPackage) {
    let currentlyEnabledKeyboards = self.defaultsRepository.readEnabledKeyboards()
    var updatedEnabledKeyboards = currentlyEnabledKeyboards

    // set enabled flag if the keyboard is contained in the set of enabledKeyboards
    installedPackage.keyboards.forEach {
      updatedEnabledKeyboards.insert($0.keyboardKey)
    }
    
    if updatedEnabledKeyboards != currentlyEnabledKeyboards {
      self.defaultsRepository.writeEnabledKeyboards(enabledKeyboardsArray: Array(updatedEnabledKeyboards))
    }
  }

  /**
   * find the installed package with the specified UUID
   */
  public func findInstalledPackage(with packageId: UUID) -> KeymanPackage? {
    guard let package = self.installedPackages.first(where: { $0.id == packageId }) else {
      print ("Error: could not find package with ID: \(packageId)")
      return nil
    }
    
    return package
  }

  /**
   * find the installed package with the specified package name
   */
  public func findInstalledPackage(with packageName: String) -> KeymanPackage? {
    guard let package = self.installedPackages.first(where: { $0.packageName == packageName }) else {
      print ("Error: could not find package with name: \(packageName)")
      return nil
    }
    
    return package
  }

  /**
   * remove/uninstall the package at the specified index
   */
  public func removeSingleKeyboardPackage(at index: Int) {
    let package = self.singleKeyboardPackages[index]
    self.removeInstalledPackage(package: package)
  }
  
  /**
   * remove/uninstall the package at the specified index
   */
  public func removeMultipleKeyboardPackage(at index: Int) {
    let package = self.multiKeyboardPackages[index]
    self.removeInstalledPackage(package: package)
  }

  /**
   * remove the installed package
   */
  func removeInstalledPackage(package: KeymanPackage) {
    // will removing this package cause the removal of any enabled keyboards?
    let removingEnabledKeyboards = !package.getEnabledKeyboardsKeys().isEmpty
    
    // delete package from disk
    self.packageRepository.deletePackage(package: package)
    
    // remove package from installed packages list
    if let index = self.installedPackages.firstIndex(where: { $0.packageName == package.packageName }) {
      self.installedPackages.remove(at: index)
    }
    
    // if we removed any enabled keyboards, then update settings
    if removingEnabledKeyboards {
      self.saveKeyboardState()
    }
  }

  /**
   * returns true if the keyboard is enabled
   * when enabled, the keyboard appears in the Keyman sub menu in the mac
   */
  public func isKeyboardEnabled(packageId: UUID, keyboardKey: String) -> Bool {
    guard let package = self.findInstalledPackage(with: packageId) else {
      print ("Could not read keyboard state for package: \(packageId) and keyboard: \(keyboardKey)")
      return false
    }
    
    let enabled = package.isKeyboardEnabled(keyboardKey: keyboardKey)    
    return enabled
  }
  
  /**
   * enable or disable the keyboard
   */
  public func setKeyboardEnabled(packageId: UUID, keyboardKey: String, enabled: Bool) {
    guard let package = self.findInstalledPackage(with: packageId) else {
      print ("Could not read keyboard state for package: \(packageId) and keyboard: \(keyboardKey)")
      return
    }
    
    print ("setKeyboardEnabled for \(keyboardKey) setting to \(enabled)")
    package.enableKeyboard(keyboardKey: keyboardKey, enabled: enabled)
    
    // update persisted state in UserDefaults enabledKeyboards array
    self.saveKeyboardState()
  }
 
  /**
   * save the keyboard state in the UserDefaults
   */
  func saveKeyboardState() {
    let enabledKeyboards = self.getEnabledKeyboardKeys()
    self.defaultsRepository.writeEnabledKeyboards(enabledKeyboardsArray: Array(enabledKeyboards))
  }
  
  /**
   *  read the Keyman packages from the group container directory and store in the installedPackages array
   */
  func loadPackages() {
    var packagesArray = nil as [KeymanPackage]?
    
    // read keyboards from disk
    packagesArray = self.packageRepository.loadAllPackages()
    
    if let persistedPackages = packagesArray {
      self.installedPackages = persistedPackages
    }
  }
  
  /**
   *  returns set containing the keyboards settings keys for all installed keyboards
   */
  func getInstalledKeyboardKeys() -> Set<String> {
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
  func getEnabledKeyboardKeys() -> Set<String> {
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
   *  ensure that UserDefaults are consistent with the installed packages
   *  remove any UserDefaults for which no installed package exists
   */
  func validateUserDefaults() {
    let installedKeyboardKeys = self.getInstalledKeyboardKeys()
    let enabledKeyboardKeys = self.defaultsRepository.readEnabledKeyboards()
    
    if (enabledKeyboardKeys.isSubset(of: installedKeyboardKeys)) {
      print("only installed keyboards are listed as enabled: no need to update defaults")
    } else {
      print("enabled keyboards list contains uninstalled keyboards: align with enabled keyboards list")
      let installedEnabledKeyboardKeys = enabledKeyboardKeys.intersection(installedKeyboardKeys)
      self.defaultsRepository.writeEnabledKeyboards(enabledKeyboardsArray: Array(installedEnabledKeyboardKeys))
    }
  }
  
  /**
   *  apply the state from the current UserDefaults to the installed packages
   */
  func applyUserDefaultsToInstalledPackages() {
    self.validateUserDefaults()
    
    let enabledKeyboards = self.defaultsRepository.readEnabledKeyboards()
    
    // set enabled flag if the keyboard is contained in the set of enabledKeyboards
    self.installedPackages.forEach { $0.keyboards.forEach
      {
        $0.enabled = enabledKeyboards.contains($0.keyboardKey)
      }
    }
  }
}
