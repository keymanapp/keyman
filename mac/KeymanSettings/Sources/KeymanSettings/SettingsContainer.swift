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
import OSLog

public enum InstallPackageError: LocalizedError {
  case packageInstallationAlreadyInProgress
  case fontCopyError
  case fontRegistrationError
  case internalError // due to invalid state, should never occur

  public var errorDescription: String? {
    switch self {
    case .packageInstallationAlreadyInProgress: return "A package installation is already in progress."
    case .fontCopyError: return "There was an error copying the font."
    case .fontRegistrationError: return "There was an error registering the font."
    case .internalError: return "An internal error occurred."
    }
  }
}

// distributed notifications
public extension Notification.Name {
  // sent from input method, received by InstallationCheck
  static let accessibilityStateResponse = Notification.Name("com.keyman.accessibility.state")
  // sent from config app (DefaultsRepository), received by input method
  static let keyboardsChanged = Notification.Name("com.keyman.keyboards.changed")
}

// in-app notifications
public extension Notification.Name {
  // sent from InstallationContainer to SettingsContainer
  static let dataMigrated = Notification.Name("com.keyman.data.migrated")
}

// define LocalizedError so that UI can present a localizable message
// when the attempt to install a KMP file using drag and drop fails
public enum DropKmpError: LocalizedError {
  case invalidFileType(String)
  case alreadyInstalled(String)
  case installFailed(String)
  case tooManyFiles
  
  public var errorDescription: String? {
    switch self {
    case .invalidFileType(let fileName): return "The file \(fileName) is not a .KMP file."
    case .alreadyInstalled(let fileName): return "The package \(fileName) is already installed."
    case .installFailed(let fileName): return "The file \(fileName) could not be installed."
    case .tooManyFiles: return "Only a single .KMP file can be installed at a time."
    }
  }
}

package let kmpFileExtension = ".kmp"
package let kmpFileExtensionWithoutDot = "kmp"

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

  // when a new package is being installed, it is tracked here
  fileprivate var packageInstall: PackageInstallHelper? = nil
  
  fileprivate let packageRepository: PackageRepo
  fileprivate let defaultsRepository: DefaultsRepo
  
  // the selected keyboard is stored in the UserDefaults
  // not indicated in the Config app but this could change
  fileprivate var selectedKeyboard: String
  
  private let keyboardSearchPrefix = "https://keyman.com/go/macos/14.0/download-keyboards"

  public var keyboardSearchUrl: URL {
    var searchUrl = URL(string: keyboardSearchPrefix)!
 
    let versionQueryItem = URLQueryItem(name:"version", value: ConfigAppUtil.configAppVersion())
    searchUrl.append(queryItems:[versionQueryItem])

    if let languageString = self.getPreferredLangauge() {
      let languageQueryItem = URLQueryItem(name:"lang", value: languageString)
      searchUrl.append(queryItems:[languageQueryItem])
    }
    Logger.setup.info("full searchUrl: \(searchUrl.absoluteString, privacy: .public)")
    return searchUrl
  }
  
  /**
   * returns the preferred language based on the user's preferences set in System Settings
   */
  private func getPreferredLangauge() -> String? {
    // returns an array of language identifiers ordered by the user's preference
    // this is independent of what the languages the app actually supports
    let systemLanguages = Locale.preferredLanguages

    var primaryLanguage: String?
    if let language = systemLanguages.first {
      primaryLanguage = language
      Logger.setup.info("primary system language: \(language, privacy: .public)")
    }

      return primaryLanguage
  }
  
  public init() {
    // initialize arrays before loading packages
    self.singleKeyboardPackages = []
    self.multiKeyboardPackages = []
    self.installedPackages = []

    // create the package repository, gaining access to the app group container directory
    do {
      try self.packageRepository = PackageRepository()
      Logger.data.log("Found documents group container")
    } catch KeymanPathError.groupContainerNotFound {
      Logger.data.error("Document group container not found")
      fatalError("Document group container not found.")
    } catch {
      Logger.data.error("Unable to access documents in group container, error \(error as NSError, privacy: .public)")
      fatalError("Unable to access documents in group container.")
    }

    // create the settings repository, gaining access to the app group UserDefaults
    do {
      try self.defaultsRepository = DefaultsRepository(suiteName: InputMethodUtil.groupId)
      Logger.data.log("Found defaults group container")
    } catch UserDefaultsError.unknownSuite {
      Logger.data.error("Defaults group container not found")
      fatalError("Defaults group container not found.")
    } catch {
      Logger.data.error("Unable to access defaults in group container, error \(error as NSError, privacy: .public)")
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
   * register observers to receive
   */
  func registerObservers() {
    NotificationCenter.default.addObserver(self, selector: #selector(self.reloadPackages), name: .dataMigrated, object: nil)
  }
  
  /**
   * Refresh the packages array and apply settings.
   * This should only be needed after a migration and could be removed if
   * the migration were made earlier.
   */
  @objc public func reloadPackages() {
    self.loadPackages()
    self.applyUserDefaultsToInstalledPackages()
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
   *  Build the URL necessary to download the specified package from the Keyman website
   */
  public func buildDownloadPackageUrl(for packageId: String) -> URL? {
    let urlString = "https://keyman.com/go/package/download/\(packageId)?platform=macos&tier=\(ConfigAppUtil.appTier)"
    
    guard let downloadPackageUrl = URL(string: urlString) else {
      return nil
    }
    
    return downloadPackageUrl
  }

  /**
   * Called when user chooses to cancel downgrade of package
   */
  public func userCanceledPackageInstallation() {
    Logger.data.log("User cancelled package installation")
    self.packageInstall?.cleanupFailedInstallation()
  
    self.packageInstall = nil
  }

  /**
   * Called when user chooses to cancel downgrade of package
   */
  public func packageInstallationFailed() {
    Logger.data.log("Package installation failed")
    self.packageInstall?.cleanupFailedInstallation()
  
    self.packageInstall = nil
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
  public func findInstalledPackage(with id: UUID) -> KeymanPackage? {
    guard let package = self.installedPackages.first(where: { $0.id == id }) else {
      Logger.setup.error("error: could not find package with UUID: \(id)")
      return nil
    }
    
    return package
  }

  /**
   * remove/uninstall the package with the specified UUID
   */
  public func removeInstalledPackage(with id: UUID) {
    if let package = findInstalledPackage(with: id) {
      self.removeInstalledPackage(package: package)
    }
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
    if let index = self.installedPackages.firstIndex(where: { $0.id == package.id }) {
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
      Logger.setup.error("isKeyboardEnabled, not read keyboard state for package: \(packageId) and keyboard: \(keyboardKey, privacy: .public)")
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
      Logger.setup.error("setKeyboardEnabled, could not read keyboard state for package: \(packageId) and keyboard: \(keyboardKey, privacy: .public)")
      return
    }
    
    Logger.setup.info("setKeyboardEnabled for \(keyboardKey, privacy: .public) setting to \(enabled)")
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
    self.installedPackages = self.packageRepository.loadAllPackages()
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
      Logger.setup.info("only installed keyboards are listed as enabled: no need to update defaults")
    } else {
      Logger.setup.info("enabled keyboards list contains uninstalled keyboards: align with enabled keyboards list")
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
  
  // MARK: Package Download and Installation
  
  /**
   * check whether an installation is already in progress
   */
  public func isInstallationInProgress() -> Bool {
    return self.packageInstall != nil
  }

  /**
   * Called by the WebView DownloadCoordinator before initiating a package download.
   * Returns a PackageInstallHelper instance to manage the state of the package being downloaded with the specified name.
   */
  public func initiateKmpFileDownload(kmpFilename: String) throws -> PackageInstallHelper? {

    guard !self.isInstallationInProgress() else {
      throw InstallPackageError.packageInstallationAlreadyInProgress
    }
    
    if let helper = self.preparePackageDownload(kmpFilename: kmpFilename) {
      self.packageInstall = helper
    }
    
    return self.packageInstall
  }

  /**
   * Creates a PackageInstallHelper instance to manage the state of the package being downloaded with the specified name.
   */
  func preparePackageDownload(kmpFilename: String) -> PackageInstallHelper? {
    return PackageInstallHelper(filename: kmpFilename, packageRepo: self.packageRepository, installedPackages: self.installedPackages, isDownload: true)
  }

  /**
   * Called by the WebView DownloadCoordinator after the download is complete.
   * Delegates to the PackageInstallHelper instance to decide whether the package should be installed.
   */
  public func packageDownloadComplete(kmpFileUrl: URL) throws {
    Logger.setup.info("packageDownloadComplete \(kmpFileUrl, privacy: .public)")

    do {
      try self.packageInstall?.prepareToInstall(for: kmpFileUrl)
    } catch {
      // clear failed download
      self.packageInstall = nil
      throw error
    }
  }

  /**
   * The package is approved for installation, so add it to the package list and update the UserDefaults for enabled keyboards
   */
  func addInstalledPackage() {
    if let package = self.packageInstall?.packageToInstall {
      self.installedPackages.append(package)
      self.addEnabledKeyboards(for: package)
    }
  }
  
  /**
   * The package is approved for installation, so replace the package of the same name in the package list.
   * Also update the UserDefaults for enabled keyboards because the new package is enabled by default, and the existing may be disabled
   */
  func replaceInstalledPackage() {
    if let package = self.packageInstall?.packageToInstall {
      // find the existing package with the same name in the installedPackages array and replace it
      // (we cannot use the id for this search, as the ids are unique)
      if let index = self.installedPackages.firstIndex(where: { $0.packageName == package.packageName }) {
        self.installedPackages[index] = package
        self.addEnabledKeyboards(for: package)
      } else {
        Logger.setup.error("Error: package '\(package.packageName, privacy: .public)' not found for replacement")
      }
    }
  }
  
  // MARK: Drag and drop Package Installation
  
  /**
   * Begin installation of a package from a KMP file.
   * Called when a .KMP file is dropped on the Configuration view
   */
  public func initiateKmpFileInstallation(at fileLocation: URL) throws -> PackageInstallHelper? {
    guard !self.isInstallationInProgress() else {
      throw InstallPackageError.packageInstallationAlreadyInProgress
    }
    
    // validate the URL of the KMP file
    try self.validateDroppedFile(from: fileLocation)
    
    let kmpFilename = fileLocation.lastPathComponent
    if let helper = self.preparePackageDrop(kmpFilename: kmpFilename) {
      self.packageInstall = helper
      do {
        try helper.prepareToInstall(for: fileLocation)
      } catch {
        // clear failed download
        self.packageInstall = nil
        throw error
      }
    }
    
    return self.packageInstall
  }

  /**
   * Install the package and add it to the installedPackages array and UserDefaults
   */
  public func installPackage() throws {
    if let install = self.packageInstall {
      do {
        try install.installPackage()
      } catch {
        self.packageInstall?.cleanupFailedInstallation()
        self.packageInstall = nil
        throw error
      }
      commitPackageInstall()
    }
  }
  
  /**
   * Update the data model for the installed package.
   */
  func commitPackageInstall() {
    if let install = self.packageInstall {

      guard let installationType = install.packageInstallationType else { return }
      
      switch installationType {
      case .newPackage:
        self.addInstalledPackage()
      case .replaceSameVersionPackage, .replaceNewerPackage, .replaceOlderPackage:
        self.replaceInstalledPackage()
      }
    }
    
    self.packageInstall = nil
  }
  
  /**
   * Creates a PackageInstallHelper instance to manage the state of the package being installed with the specified name.
   */
  func preparePackageDrop(kmpFilename: String) -> PackageInstallHelper? {
    return PackageInstallHelper(filename: kmpFilename, packageRepo: self.packageRepository, installedPackages: self.installedPackages, isDownload: false)
  }

  /**
   * Validate the URL for the file we are dropping
   */
  func validateDroppedFile(from fileLocation: URL) throws {
    // if the file does not end with .kmp, reject it
    if fileLocation.pathExtension.lowercased() != kmpFileExtensionWithoutDot {
      throw DropKmpError.invalidFileType(fileLocation.lastPathComponent)
    }
  }
}
