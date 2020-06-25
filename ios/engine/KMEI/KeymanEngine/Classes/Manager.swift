//
//  Manager.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit
import WebKit
import XCGLogger
import DeviceKit
import Reachability

typealias FetchKeyboardsBlock = ([String: Any]?) -> Void

// MARK: - Constants

// Possible states that a keyboard or lexical model can be in
public enum KeyboardState {
  case needsDownload
  case needsUpdate
  case upToDate
  case downloading
  case none
}

public enum VibrationSupport {
  case none // Has no vibrator
  case basic // Has only the basic 0.4 sec long vibration
  case basic_plus // Has undocumented access to three other vibration lengths
  case taptic // Has the Taptic engine, allowing use of UIImpactFeedbackGenerator for customizable vibrations
}

// Strings
private let keyboardChangeHelpText = "Tap here to change keyboard"

// URLs - used for reachability test
private let keymanHostName = "api.keyman.com"

public class Manager: NSObject, UIGestureRecognizerDelegate {
  /// Application group identifier for shared container. Set this before accessing the shared manager.
  public static var applicationGroupIdentifier: String?

  public static let shared = Manager()

  public var fileBrowserLauncher: ((UINavigationController) -> Void)? = nil

  /// Display the help bubble on first use.
  public var isKeymanHelpOn = true
  
  public var isSystemKeyboard: Bool {
    guard let kbd = self._inputViewController else {
      return false
    }

    return kbd.isSystemKeyboard
  }

  // TODO: Change API to not disable removing as well
  /// Allow users to add new keyboards in the keyboard picker.
  ///  - Default value is true.
  ///  - Setting this to false will also disable keyboard removal. To enable keyboard removal you should set
  ///    canRemoveKeyboards to true.
  public var canAddNewKeyboards: Bool {
    get {
      return _canAddNewKeyboards
    }
    set(canAddNewKeyboards) {
      _canAddNewKeyboards = canAddNewKeyboards
      if !canAddNewKeyboards {
        canRemoveKeyboards = false
      }
    }
  }
  private var _canAddNewKeyboards = true

  /// Allow users to remove keyboards.
  /// - Default value is true.
  /// - The default keyboard is additionally prevented from being removed by canRemoveDefaultkeyboard.
  public var canRemoveKeyboards = true

  /// Allow the default keyboard to be removed.
  /// The last keyboard cannot be removed, so the default keyboard cannot be removed if it
  /// is the only keyboard in the list, regardless of the value of this property.
  /// The default value is false.
  public var canRemoveDefaultKeyboard = false

  public let apiKeyboardRepository: APIKeyboardRepository
    
    
  // TODO: Change API to not disable removing as well
  /// Allow users to add new lexical models in the lexical model picker.
  ///  - Default value is true.
  ///  - Setting this to false will also disable lexical model removal. To enable lexical model removal you should set
  ///    canRemoveLexicalModels to true.
  public var canAddNewLexicalModels: Bool {
    get {
      return _canAddNewLexicalModels
    }
    set(canAddNewLexicalModels) {
      _canAddNewLexicalModels = canAddNewLexicalModels
      if !canAddNewLexicalModels {
        canRemoveLexicalModels = false
      }
    }
  }
  private var _canAddNewLexicalModels = true
  
  /// Allow users to remove lexical models.
  /// - Default value is true.
  /// - The default lexical model is additionally prevented from being removed by canRemoveDefaultLexicalModel.
  public var canRemoveLexicalModels = true
  
  /// Allow the default lexical model to be removed.
  /// The last lexical model CAN be removed, as this is an optional feature
  /// The default value is true.
  public var canRemoveDefaultLexicalModel = true
  
  public let apiLexicalModelRepository: APILexicalModelRepository

  /// In keyboard extensions (system keyboard), `UIApplication.openURL(_:)` is unavailable. The API is not called in
  /// the system keyboard since `KeyboardInfoViewController` is never used. `openURL(:_)` is only used in applications,
  /// where it is safe. However, the entire Keyman Engine framework must be compiled with extension-safe APIs.
  ///
  /// Set this to `UIApplication.shared.openURL` in your application.
  public var openURL: ((URL) -> Bool)?

  var currentKeyboardID: FullKeyboardID?
  private var _currentLexicalModelID: FullLexicalModelID?
  var currentLexicalModelID: FullLexicalModelID? {
    get {
      if _currentLexicalModelID == nil {
        let userData = Util.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
        _currentLexicalModelID = userData.currentLexicalModelID
      }
      return _currentLexicalModelID
    }
    
    set(value) {
      _currentLexicalModelID = value
      let userData = Util.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
      userData.currentLexicalModelID = _currentLexicalModelID
      userData.synchronize()
    }
  }

  var shouldReloadKeyboard = false
  var shouldReloadLexicalModel = false

  var _inputViewController: InputViewController?
  var currentResponder: KeymanResponder?
  
  // This allows for 'lazy' initialization of the keyboard.
  open var inputViewController: InputViewController! {
    get {
      // Occurs for the in-app keyboard ONLY.
      if _inputViewController == nil {
        _inputViewController = InputViewController(forSystem: false)
      }
      return _inputViewController
    }

    set(value) {
      _inputViewController = value
    }
  }

  //private var downloadQueue: HTTPDownloader?
  private var reachability: Reachability!
  var didSynchronize = false

  // MARK: - Object Admin

  private override init() {
    apiKeyboardRepository = APIKeyboardRepository()
    apiLexicalModelRepository = APILexicalModelRepository()
    super.init()

    URLProtocol.registerClass(KeymanURLProtocol.self)

    Migrations.migrate(storage: Storage.active)
    Migrations.updateResources(storage: Storage.active)

    if Storage.active.userDefaults.userKeyboards?.isEmpty ?? true {
      Storage.active.userDefaults.userKeyboards = [Defaults.keyboard]

      // Ensure the default keyboard is installed in this case.
      do {
        try Storage.active.installDefaultKeyboard(from: Resources.bundle)
      } catch {
        log.error("Failed to copy default keyboard from bundle: \(error)")
      }
    }
    Migrations.engineVersion = Version.latestFeature

    if Util.isSystemKeyboard || Storage.active.userDefaults.bool(forKey: Key.keyboardPickerDisplayed) {
      isKeymanHelpOn = false
    }

    do {
      try Storage.active.copyKMWFiles(from: Resources.bundle)
    } catch {
      log.error("Failed to copy KMW files from bundle: \(error)")
    }

    updateUserKeyboards(with: Defaults.keyboard)

    do {
      try reachability = Reachability(hostname: keymanHostName)
    } catch {
      log.error("Could not start Reachability object: \(error)")
    }

    if(!Util.isSystemKeyboard) {
      NotificationCenter.default.addObserver(self, selector: #selector(self.reachabilityChanged),
                                           name: .reachabilityChanged, object: reachability)
      do {
        try reachability.startNotifier()
      } catch {
        log.error("failed to start Reachability notifier: \(error)")
      }
    }

    // We used to preload the old KeymanWebViewController, but now that it's embedded within the
    // InputViewController, that's not exactly viable.
  }

  // MARK: - Keyboard management
  
  public func showKeymanEngineSettings(inVC: UIViewController) -> Void {
    hideKeyboard()
    
    // Allows us to set a custom UIToolbar for download/update status displays.
    let nc = UINavigationController(navigationBarClass: nil, toolbarClass: ResourceDownloadStatusToolbar.self)
    
    // Grab our newly-generated toolbar instance and inform it of its parent NavigationController.
    // This will help streamline use of the 'toolbar' as a status/update bar.
    let toolbar = nc.toolbar as? ResourceDownloadStatusToolbar
    toolbar?.navigationController = nc
    
    // As it's the first added view controller, settingsVC will function as root automatically.
    let settingsVC = SettingsViewController()
    nc.pushViewController(settingsVC, animated: false)
    nc.modalTransitionStyle = .coverVertical
    nc.modalPresentationStyle = .pageSheet
    inVC.present(nc, animated: true)
  }

  /// Sets the current keyboard, querying from the user's list of keyboards.
  ///
  /// - Precondition:
  ///   - The keyboard must be added with `addKeyboard()`.
  ///
  /// - SeeAlso:
  ///   - addKeyboard()
  /// - Returns: Whether the keyboard was set successfully
  //TODO: this method appears unused, should we remove it?
  public func setKeyboard(withFullID fullID: FullKeyboardID) -> Bool {
    if let keyboard = Storage.active.userDefaults.userKeyboard(withFullID: fullID) {
        return setKeyboard(keyboard)
    }
    return false
  }
  
  // returns lexical model id, given language id
  func preferredLexicalModel(_ ud: UserDefaults, forLanguage lgCode: String) -> InstallableLexicalModel? {
    if let preferredID = ud.preferredLexicalModelID(forLanguage: lgCode) {
      // We need to match both the model id and the language code - registration fails
      // when the language code mismatches the current keyboard's set code!
      return ud.userLexicalModels?.first { $0.id == preferredID && $0.languageID == lgCode }
    }
    return nil
  }
  

  /// Set the current keyboard.
  ///
  /// - Throws: error if the keyboard was unchanged
  public func setKeyboard(_ kb: InstallableKeyboard) -> Bool {
    // KeymanWebViewController relies upon this method to activate the keyboard after a page reload,
    // and as a system keyboard, the controller is rebuilt each time the keyboard is loaded.
    //
    // We MUST NOT shortcut this method as a result; doing so may (rarely) result in the infamous
    // blank keyboard bug!
    if kb.fullID == currentKeyboardID && !self.isSystemKeyboard {
      log.info("Keyboard unchanged: \(kb.fullID)")
      return false
     // throw KeyboardError.unchanged
    }

    log.info("Setting language: \(kb.fullID)")

    currentKeyboardID = kb.fullID

    if let fontFilename = kb.font?.source.first(where: { $0.hasFontExtension }) {
      _ = FontManager.shared.registerFont(at: Storage.active.fontURL(forResource: kb, filename: fontFilename)!)
    }
    if let oskFontFilename = kb.oskFont?.source.first(where: { $0.hasFontExtension }) {
      _ = FontManager.shared.registerFont(at: Storage.active.fontURL(forResource: kb, filename: oskFontFilename)!)
    }

    inputViewController.setKeyboard(kb)

    let userData = Util.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
    userData.currentKeyboardID = kb.fullID
    userData.synchronize()

    if isKeymanHelpOn {
      inputViewController.showHelpBubble(afterDelay: 1.5)
    }
    
    // getAssociatedLexicalModel(langID) and setLexicalModel
    
    NotificationCenter.default.post(name: Notifications.keyboardChanged,
                                    object: self,
                                    value: kb)

    // Now to handle lexical model + banner management
    inputViewController.clearModel()
    
    let userDefaults: UserDefaults = Storage.active.userDefaults
    // If we have a lexical model for the keyboard's language, activate it.
    if let preferred_model = preferredLexicalModel(userDefaults, forLanguage: kb.languageID) {
      _ = Manager.shared.registerLexicalModel(preferred_model)
    } else if let first_model = userDefaults.userLexicalModels?.first(where: { $0.languageID == kb.languageID }) {
      _ = Manager.shared.registerLexicalModel(first_model)
    }
    
    inputViewController.fixLayout()
    
    return true
  }

  /// Adds a new keyboard to the list in the keyboard picker if it doesn't already exist.
  /// The keyboard must be downloaded (see `downloadKeyboard()`) or preloaded (see `preloadLanguageFile()`)
  @available(*, deprecated, message: "Deprecated in favor of ResourceFileManager.install(resourceWithID:from:)")
  public func addKeyboard(_ keyboard: InstallableKeyboard) {
    var kbdToInstall = keyboard

    // 3rd party installation of keyboards (13.0 and before):
    // - Manager.shared.preloadFiles was called first to import the file resources
    // - Then Manager.shared.addKeyboard installs the keyboard.
    //
    // Since 3rd-party uses never provided kmp.json files, we need to instant-migrate
    // them here.
    if !Migrations.resourceHasPackageMetadata(keyboard) {
      let wrappedKbds = Migrations.migrateToKMPFormat([keyboard])
      guard wrappedKbds.count == 1 else {
        log.error("Could not properly import keyboard")
        return
      }
      kbdToInstall = wrappedKbds[0]
    } else {
      ResourceFileManager.shared.addResource(kbdToInstall)
    }
  }
    
    
  /// Sets the current lexical model, querying from the user's list of lexical models.
  ///
  /// - Precondition:
  ///   - The lexical model must be added with `addLexicalModel()`.
  ///
  /// - SeeAlso:
  ///   - addLexicalModel()
  /// - Returns: Whether the lexical model was set successfully
  //TODO: this method appears unused, should we remove it?
  public func registerLexicalModel(withFullID fullID: FullLexicalModelID) -> Bool {
    if let lexicalModel = Storage.active.userDefaults.userLexicalModel(withFullID: fullID) {
      return registerLexicalModel(lexicalModel)
    }
    return false
  }
  
  /// Registers a lexical model with KMW.
  public func registerLexicalModel(_ lm: InstallableLexicalModel) -> Bool {
    log.info("Setting lexical model: \(lm.fullID)")
    
    currentLexicalModelID = lm.fullID
    
    inputViewController.registerLexicalModel(lm)
    
    if isKeymanHelpOn {
      inputViewController.showHelpBubble(afterDelay: 1.5)
    }
    
    // While this does only register the model with KMW, the timing of this generally does
    // result in a change of the actual model.
    NotificationCenter.default.post(name: Notifications.lexicalModelChanged,
                                    object: self,
                                    value: lm)
    
    return true
  }
  
  /** Adds a new lexical model to the list in the lexical model picker if it doesn't already exist.
  *   The lexical model must be downloaded (see `downloadLexicalModel()`) or preloaded (see `preloadLanguageFile()`)
  *   I believe this is background-thread-safe (no UI done)
  */
  @available(*, deprecated, message: "Deprecated in favor of ResourceFileManager.install(resourceWithID:from:)")
  static public func addLexicalModel(_ lexicalModel: InstallableLexicalModel) {
    var modelToInstall = lexicalModel

    // Potential 3rd party installation of lexical models (12.0, 13.0):
    // - Manager.shared.preloadFiles was called first to import the file resources
    // - Then Manager.addLexicalModel installs the lexical model.
    //
    // Since 3rd-party uses never provided kmp.json files, we need to instant-migrate
    // them here.
    if !Migrations.resourceHasPackageMetadata(lexicalModel) {
      let wrappedModels = Migrations.migrateToKMPFormat([lexicalModel])
      guard wrappedModels.count == 1 else {
        log.error("Could not properly import lexical model")
        return
      }
      modelToInstall = wrappedModels[0]
    } else {
      ResourceFileManager.shared.addResource(modelToInstall)
    }
  }

  /// Removes a keyboard from the list in the keyboard picker if it exists.
  /// - Returns: The keyboard exists and was removed
  public func removeKeyboard(withFullID fullID: FullKeyboardID) -> Bool {
    // Remove keyboard from the list if it exists
    let index = Storage.active.userDefaults.userKeyboards?.firstIndex { $0.fullID == fullID }
    if let index = index {
      return removeKeyboard(at: index)
    }
    return false
  }

  /// Removes the keyboard at index from the keyboards list if it exists.
  /// - Returns: The keyboard exists and was removed
  public func removeKeyboard(at index: Int) -> Bool {
    let userData = Storage.active.userDefaults

    // If user defaults for keyboards list does not exist, do nothing.
    guard var userKeyboards = userData.userKeyboards else {
      return false
    }

    guard index < userKeyboards.count else {
      return false
    }

    let kb = userKeyboards[index]
    userKeyboards.remove(at: index)
    userData.userKeyboards = userKeyboards
    userData.set([Date()], forKey: Key.synchronizeSWKeyboard)
    userData.synchronize()

    log.info("Removing keyboard with ID \(kb.id) and languageID \(kb.languageID)")

    // Set a new keyboard if deleting the current one
    if kb.fullID == currentKeyboardID {
      _ = setKeyboard(userKeyboards[0])
    }

    if !userKeyboards.contains(where: { $0.id == kb.id }) {
      // TODO:  should make sure that there are no resources in the package,
      //        rather than just 'no matching keyboards'.
      let keyboardDir = Storage.active.resourceDir(for: kb)!
      FontManager.shared.unregisterFonts(in: keyboardDir, fromSystemOnly: false)
      log.info("Deleting directory \(keyboardDir)")
      if (try? FileManager.default.removeItem(at: keyboardDir)) == nil {
        log.error("Failed to delete \(keyboardDir)")
      }
    } else {
      log.info("User has another language installed. Skipping delete of keyboard files.")
    }

    NotificationCenter.default.post(name: Notifications.keyboardRemoved, object: self, value: kb)
    return true
  }

  /// - Returns: Info for the current keyboard, if a keyboard is set
  public var currentKeyboard: InstallableKeyboard? {
    guard let fullID = currentKeyboardID else {
      return nil
    }
    return Storage.active.userDefaults.userKeyboard(withFullID: fullID)
  }
    
  /// Removes a lexical model from the list in the lexical model picker if it exists.
  /// - Returns: The lexical model exists and was removed
  public func removeLexicalModel(withFullID fullID: FullLexicalModelID) -> Bool {
    // Remove lexical model from the list if it exists
    let index = Storage.active.userDefaults.userLexicalModels?.firstIndex { $0.fullID == fullID }
    if let index = index {
      return removeLexicalModel(at: index)
    }
    return false
  }
  
  /// Removes the lexical model at index from the lexical models list if it exists.
  public func removeLexicalModelFromUserList(userDefs ud: UserDefaults, at index: Int) -> InstallableLexicalModel? {
    // If user defaults for lexical models list does not exist, do nothing.
    guard var userLexicalModels = ud.userLexicalModels else {
      return nil
    }
    
    guard index < userLexicalModels.count else {
      return nil
    }
    
    let lm = userLexicalModels[index]
    log.info("Removing lexical model with ID \(lm.id) and languageID \(lm.languageID) from user list of all models")
    userLexicalModels.remove(at: index)
    ud.userLexicalModels = userLexicalModels
    ud.set([Date()], forKey: Key.synchronizeSWLexicalModel)
    ud.synchronize()
    return lm
}
  
  /// Removes the lexical model at index from the lexical models list if it exists.
  public func removeLexicalModelFromLanguagePreference(userDefs ud: UserDefaults, _ lm: InstallableLexicalModel) {
    log.info("Removing lexical model with ID \(lm.id) and languageID \(lm.languageID) from per-language prefs")
    ud.set(preferredLexicalModelID: nil, forKey: lm.languageID)
  }

  /// Removes the lexical model at index from the lexical models list if it exists.
  /// - Returns: The lexical model exists and was removed
  public func removeLexicalModel(at index: Int) -> Bool {
    let userData = Storage.active.userDefaults
    guard let lm = removeLexicalModelFromUserList(userDefs: userData, at: index) else {
      return false
    }

    removeLexicalModelFromLanguagePreference(userDefs: userData, lm)
    inputViewController.deregisterLexicalModel(lm);
    // Set a new lexical model if deleting the current one
    let userLexicalModels = userData.userLexicalModels! //removeLexicalModelFromUserList fails above if this is not present

    if lm.fullID == currentLexicalModelID {
      if let first_lm = userLexicalModels.first(where: {$0.languageID == lm.languageID}) {
        _ = registerLexicalModel(first_lm)
      } else {
        log.info("no more lexical models available for language \(lm.fullID)")
        currentLexicalModelID = nil
      }
    }
    
    if !userLexicalModels.contains(where: { $0.id == lm.id }) {
      let lexicalModelDir = Storage.active.resourceDir(for: lm)!
      FontManager.shared.unregisterFonts(in: lexicalModelDir, fromSystemOnly: false)
      log.info("Deleting directory \(lexicalModelDir)")
      if (try? FileManager.default.removeItem(at: lexicalModelDir)) == nil {
        log.error("Failed to delete \(lexicalModelDir)")
      }
    } else {
      log.info("User has another language installed. Skipping delete of lexical model files.")
    }
    
    NotificationCenter.default.post(name: Notifications.lexicalModelRemoved, object: self, value: lm)
    return true
  }
  
  /// - Returns: Info for the current lexical model, if a lexical model is set
  public var currentLexicalModel: InstallableLexicalModel? {
    guard let fullID = currentLexicalModelID else {
      return nil
    }
    return Storage.active.userDefaults.userLexicalModel(withFullID: fullID)
  }

  /// Switch to the next keyboard.
  /// - Returns: Index of the newly selected keyboard.
  public func switchToNextKeyboard() -> Int? {
    guard let userKeyboards = Storage.active.userDefaults.userKeyboards,
      let index = userKeyboards.firstIndex(where: { self.currentKeyboardID == $0.fullID })
    else {
      return nil
    }
    let newIndex = (index + 1) % userKeyboards.count
    _ = setKeyboard(userKeyboards[newIndex])
    return newIndex
  }

  /// - Returns: The font name for the given keyboard ID and languageID, or returns nil if
  ///   - The keyboard doesn't have a font
  ///   - The keyboard info is not available in the user keyboards list
  public func fontNameForKeyboard(withFullID fullID: FullKeyboardID) -> String? {
    if let kb = Storage.active.userDefaults.userKeyboard(withFullID: fullID),
       let filename = kb.font?.source.first(where: { $0.hasFontExtension }) {
      let fontURL = Storage.active.fontURL(forResource: kb, filename: filename)!
      return FontManager.shared.fontName(at: fontURL)
    }
    return nil
  }

  /// - Returns: the OSK font name for the given keyboard ID and languageID, or returns nil if
  ///   - The keyboard doesn't have an OSK font
  ///   - The keyboard info is not available in the user keyboards list
  func oskFontNameForKeyboard(withFullID fullID: FullKeyboardID) -> String? {
    if let kb = Storage.active.userDefaults.userKeyboard(withFullID: fullID),
       let filename = kb.oskFont?.source.first(where: { $0.hasFontExtension }) {
      let fontURL = Storage.active.fontURL(forResource: kb, filename: filename)!
      return FontManager.shared.fontName(at: fontURL)
    }
    return nil
  }
    
  // MARK: - Adhoc keyboards
  public func parseKbdKMP(_ folder: URL, isCustom: Bool) throws -> Void {
    guard let kmp = KeymanPackage.parse(folder) as? KeyboardKeymanPackage else {
      throw KMPError.wrongPackageType
    }

    for resourceSet in kmp.installables {
      for resource in resourceSet {
        try ResourceFileManager.shared.install(resourceWithID: resource.fullID, from: kmp)
        // Install the keyboard for only the first language pairing defined in the package.
        break
      }
    }
  }
    
  // MARK: - Adhoc lexical models
  static public func parseLMKMP(_ folder: URL, isCustom: Bool) throws -> Void {
    guard let kmp = KeymanPackage.parse(folder) as? LexicalModelKeymanPackage else {
      throw KMPError.wrongPackageType
    }

    try kmp.installables.forEach { resourceSet in
      try resourceSet.forEach { resource in
        try ResourceFileManager.shared.install(resourceWithID: resource.fullID, from: kmp)
      }
    }
  }
  
  @objc func reachabilityChanged(_ notification: Notification) {
    log.debug {
      let reachStr: String
      switch reachability.connection {
      case Reachability.Connection.wifi:
        reachStr = "Reachable Via WiFi"
      case Reachability.Connection.cellular:
        reachStr = "Reachable Via WWan"
      default:
        reachStr = "Not Reachable"
      }
      return "Reachability changed to '\(reachStr)'"
    }
  }

  // MARK: - Loading custom keyboards
  /// Preloads the JS and font files required for a keyboard.
  @available(*, deprecated, message: "Use ResourceFileManager's methods to install resources from KMPs.")
  public func preloadFiles(forKeyboardID keyboardID: String, at urls: [URL], shouldOverwrite: Bool) throws {
    let keyboardDir = Storage.active.legacyKeyboardDir(forID: keyboardID)
    try FileManager.default.createDirectory(at: keyboardDir, withIntermediateDirectories: true)
    for url in urls {
      try Storage.copy(at: url,
                       to: keyboardDir.appendingPathComponent(url.lastPathComponent),
                       shouldOverwrite: shouldOverwrite,
                       excludeFromBackup: true)
    }
  }

  // MARK: - File system and UserData management

  /// Updates the user's installed keyboards and current keyboard with information in newKeyboard.
  /// - Parameter newKeyboard: Info for updated keyboard.
  func updateUserKeyboards(with newKeyboard: InstallableKeyboard) {
    let userData = Storage.active.userDefaults
    guard var userKeyboards = userData.userKeyboards else {
      return
    }

    // Set version in user keyboards list
    for i in userKeyboards.indices {
      var kb = userKeyboards[i]
      if kb.id == newKeyboard.id {
        if kb.languageID == newKeyboard.languageID {
          kb = newKeyboard
        } else {
          kb.version = newKeyboard.version
        }
        userKeyboards[i] = kb
      }
    }
    userData.userKeyboards = userKeyboards
    userData.synchronize()
  }
    
  /// Updates the user's installed lexical models and current lexical model with information in newLexicalModel.
  /// - Parameter newLexicalModel: Info for updated lexical model.
  func updateUserLexicalModels(with newLexicalModel: InstallableLexicalModel) {
    let userData = Storage.active.userDefaults
    guard var userLexicalModels = userData.userLexicalModels else {
      return
    }
    
    // Set version in user lexical models list
    for i in userLexicalModels.indices {
      var lm = userLexicalModels[i]
      if lm.id == newLexicalModel.id {
        if lm.languageID == newLexicalModel.languageID {
          lm = newLexicalModel
        } else {
          lm.version = newLexicalModel.version
        }
        userLexicalModels[i] = lm
      }
    }
    userData.userLexicalModels = userLexicalModels
    userData.synchronize()
  }

  func synchronizeSWKeyboard() {
    if let shared = Storage.shared,
      let nonShared = Storage.nonShared {
      let keysToCopy = [Key.userKeyboardsList, Key.engineVersion]
      shared.copyUserDefaults(to: nonShared, withKeys: keysToCopy, shouldOverwrite: true)
      do {
        try shared.copyFiles(to: nonShared)
        FontManager.shared.registerCustomFonts()
      } catch {
        log.error("Failed to copy from shared container: \(error)")
      }
    }
  }

  // MARK: - View management

  /// Displays a list of available keyboards and allows a user to add/download new keyboards
  /// or remove existing ones.
  ///
  /// - Parameters:
  ///   - in: The current UIViewController (recommended) or the navigation controller
  ///   - shouldAddKeyboard: Whether to immediately open the view to add a new keyboard
  /// - SeeAlso:
  /// TextView/TextField to enable/disable the keyboard picker
  public func showKeyboardPicker(in viewController: UIViewController, shouldAddKeyboard: Bool) {
    hideKeyboard()
    let vc = KeyboardSwitcherViewController()
    let nc = UINavigationController(rootViewController: vc)
    nc.modalTransitionStyle = .coverVertical
    nc.modalPresentationStyle = .pageSheet
    viewController.present(nc, animated: true) {() -> Void in
      if shouldAddKeyboard {
        vc.showAddKeyboard()
      } else {
        let userData = Storage.active.userDefaults
        userData.set(true, forKey: Key.keyboardPickerDisplayed)
        userData.synchronize()
        self.isKeymanHelpOn = false
      }
    }
  }

  public func dismissKeyboardPicker(_ viewController: UIViewController) {
    // #1045 - Setting animated to false "fixes" the display problems and prevents the crash (on iPad 10.5"
    // and 12.9"), but it makes the transition less smooth (obviously) and probably isn't the "right"
    // way to fix the problem. Presumably there is some kind of underlying plumbing issue that is the
    // true source of the problems.
    viewController.dismiss(animated: false)
    showKeyboard()
    if shouldReloadKeyboard {
      inputViewController.reload()
    }
    NotificationCenter.default.post(name: Notifications.keyboardPickerDismissed, object: self, value: ())
  }
    
  public func dismissLexicalModelPicker(_ viewController: UIViewController) {
    // #1045 - Setting animated to false "fixes" the display problems and prevents the crash (on iPad 10.5"
    // and 12.9"), but it makes the transition less smooth (obviously) and probably isn't the "right"
    // way to fix the problem. Presumably there is some kind of underlying plumbing issue that is the
    // true source of the problems.
    viewController.dismiss(animated: false)
    showKeyboard()
    if shouldReloadLexicalModel {
      inputViewController.reload()
    }
    NotificationCenter.default.post(name: Notifications.lexicalModelPickerDismissed, object: self, value: ())
  }

  // MARK: - Text

  public func showKeyboard() {
    currentResponder?.summonKeyboard()
  }
  
  public func hideKeyboard() {
    currentResponder?.dismissKeyboard()
    Manager.shared.inputViewController.resetKeyboardState()
  }

  func clearText() {
    inputViewController.clearText()
  }
  
  func resetContext() {
    inputViewController.resetContext()
  }

  func setContextState(text: String?, range: NSRange) {
    inputViewController.setContextState(text: text, range: range)
  }
  
  var vibrationSupportLevel: VibrationSupport {
    let device = Device.current

    if device.isPod {
      return .none
    } else if device.isPad {
      // May not be entirely true, but I can't find any documentation on which
      // ones DO have it.  Closest thing is https://discussions.apple.com/thread/7415858
      // that suggests none before Jan 2016 had it, at least.
      return .none
    } else if device.isPhone {
      let basicVibrationModels: [Device] = [.iPhone4, .iPhone4s, .iPhone5, Device.iPhone5s,
                                            .iPhone5c, .iPhone6, .iPhone6Plus, .iPhoneSE]
      if device.isOneOf(Device.allSimulators) {
        // The Simulator for testing on macOS doesn't emulate or indicate vibration, unfortunately.
        return .none
      } else if device == Device.iPhone6s || device == Device.iPhone6sPlus {
        return .basic_plus
      } else if device.isOneOf(basicVibrationModels) {
        return .basic
      } else {
        return .taptic
      }
    } else {
      return .none
    }
  }

  /*-----------------------------
   *    Legacy API endpoints    -
   *-----------------------------
   *
   * Some functionality has been refactored into separate classes for 12.0.
   * No reason we can't add a couple of helper functions to help forward
   * the data for apps built on older versions of KMEI, though.
   */

  public func downloadKeyboard(withID: String, languageID: String, isUpdate: Bool, fetchRepositoryIfNeeded: Bool = true) {
    let kbdFullID = FullKeyboardID(keyboardID: withID, languageID: languageID)
    let completionBlock = ResourceDownloadManager.shared.standardKeyboardInstallCompletionBlock(forFullID: kbdFullID, withModel: true)
    ResourceDownloadManager.shared.downloadKeyboard(withID: withID,
                                                    languageID: languageID,
                                                    isUpdate: isUpdate,
                                                    fetchRepositoryIfNeeded: fetchRepositoryIfNeeded,
                                                    completionBlock: completionBlock)
  }

  // A new API, but it so closely parallels downloadKeyboard that we should add a 'helper' handler here.
  public func downloadLexicalModel(withID: String, languageID: String, isUpdate: Bool, fetchRepositoryIfNeeded: Bool = true) {
    let lmFullID = FullLexicalModelID(lexicalModelID: withID, languageID: languageID)
    let completionBlock = ResourceDownloadManager.shared.standardLexicalModelInstallCompletionBlock(forFullID: lmFullID)
    ResourceDownloadManager.shared.downloadLexicalModel(withID: withID,
                                                        languageID: languageID,
                                                        isUpdate: isUpdate,
                                                        fetchRepositoryIfNeeded: fetchRepositoryIfNeeded,
                                                        completionBlock: completionBlock)
  }

  public func stateForKeyboard(withID keyboardID: String) -> KeyboardState {
    return ResourceDownloadManager.shared.stateForKeyboard(withID: keyboardID)
  }

  // Technically new, but it does closely parallel an old API point.
  public func stateForLexicalModel(withID modelID: String) -> KeyboardState {
    return ResourceDownloadManager.shared.stateForLexicalModel(withID: modelID)
  }

  public func parseKMP(_ folder: URL, type: LanguageResourceType = .keyboard, isCustom: Bool) throws -> Void {
    switch type {
      case .keyboard:
        try! parseKbdKMP(folder, isCustom: isCustom)
        break
      case .lexicalModel:
        // Yep.  Unlike the original, THIS one is static.
        try! Manager.parseLMKMP(folder, isCustom: isCustom)
        break
    }
  }

  // To re-implement the old API (missing from v11!) we need to pick one version of height.
  // For library users, the total height of the InputViewController should be most useful.
  public func keyboardHeight() -> CGFloat {
    return inputViewController?.expandedHeight ?? 0
  }
}
