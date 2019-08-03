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
import Zip
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

public class Manager: NSObject, HTTPDownloadDelegate, UIGestureRecognizerDelegate {
  /// Application group identifier for shared container. Set this before accessing the shared manager.
  public static var applicationGroupIdentifier: String?

  public static let shared = Manager()

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

  var currentRequest: HTTPDownloadRequest?
  var shouldReloadKeyboard = false
  var shouldReloadLexicalModel = false

  var _inputViewController: InputViewController?
  var currentResponder: KeymanResponder?
  
  // This allows for 'lazy' initialization of the keyboard.
  var inputViewController: InputViewController! {
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

  private var downloadQueue: HTTPDownloader?
  private var sharedQueue: HTTPDownloader!
  private var reachability: Reachability!
  var didSynchronize = false

  // MARK: - Object Admin
  deinit {
    NotificationCenter.default.removeObserver(self)
    // FIXME: Likely unneeded unless a reference exists to currentRequest outside of Manager
    if let currentRequest = currentRequest {
      currentRequest.userInfo["completionBlock"] = nil
    }
  }

  private override init() {
    apiKeyboardRepository = APIKeyboardRepository()
    apiLexicalModelRepository = APILexicalModelRepository()
    super.init()

    URLProtocol.registerClass(KeymanURLProtocol.self)

    Migrations.migrate(storage: Storage.active)
    if Storage.active.userDefaults.userKeyboards?.isEmpty ?? true {
      Storage.active.userDefaults.userKeyboards = [Defaults.keyboard]
    }

    if Util.isSystemKeyboard || Storage.active.userDefaults.bool(forKey: Key.keyboardPickerDisplayed) {
      isKeymanHelpOn = false
    }

    do {
      try Storage.active.copyKMWFiles(from: Resources.bundle)
    } catch {
      log.error("Failed to copy KMW files from bundle: \(error)")
    }

    updateUserKeyboards(with: Defaults.keyboard)

    reachability = Reachability(hostname: keymanHostName)

    if(!Util.isSystemKeyboard) {
      NotificationCenter.default.addObserver(self, selector: #selector(self.reachabilityChanged),
                                           name: .reachabilityChanged, object: reachability)
      do {
        try reachability.startNotifier()
      } catch {
        log.error("failed to start Reachability notifier: \(error)")
      }
    }

    /* HTTPDownloader only uses this for its delegate methods.  So long as we don't
     * set the queue running, this should be perfectly fine.
     */
    sharedQueue = HTTPDownloader.init(self)

    // We used to preload the old KeymanWebViewController, but now that it's embedded within the
    // InputViewController, that's not exactly viable.
  }

  // MARK: - Keyboard management
  
  public func showKeymanEngineSettings(inVC: UIViewController) -> Void {
    hideKeyboard()
    let settingsVC = SettingsViewController()
    let nc = UINavigationController(rootViewController: settingsVC)
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
    if kb.fullID == currentKeyboardID {
      log.info("Keyboard unchanged: \(kb.fullID)")
      return false
     // throw KeyboardError.unchanged
    }

    log.info("Setting language: \(kb.fullID)")

    currentKeyboardID = kb.fullID

    if let fontFilename = kb.font?.source.first(where: { $0.hasFontExtension }) {
      _ = FontManager.shared.registerFont(at: Storage.active.fontURL(forKeyboardID: kb.id, filename: fontFilename))
    }
    if let oskFontFilename = kb.oskFont?.source.first(where: { $0.hasFontExtension }) {
      _ = FontManager.shared.registerFont(at: Storage.active.fontURL(forKeyboardID: kb.id, filename: oskFontFilename))
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
  public func addKeyboard(_ keyboard: InstallableKeyboard) {
    let keyboardPath = Storage.active.keyboardURL(for: keyboard).path
    if !FileManager.default.fileExists(atPath: keyboardPath) {
      log.error("Could not add keyboard with ID: \(keyboard.id) because the keyboard file does not exist")
      return
    }

    // Get keyboards list if it exists in user defaults, otherwise create a new one
    let userDefaults = Storage.active.userDefaults
    var userKeyboards = userDefaults.userKeyboards ?? []

    // Update keyboard if it exists
    if let index = userKeyboards.index(where: { $0.fullID == keyboard.fullID }) {
      userKeyboards[index] = keyboard
    } else {
      userKeyboards.append(keyboard)
    }

    userDefaults.userKeyboards = userKeyboards
    userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
    userDefaults.synchronize()
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
  public func addLexicalModel(_ lexicalModel: InstallableLexicalModel) {
    let lexicalModelPath = Storage.active.lexicalModelURL(for: lexicalModel).path
    if !FileManager.default.fileExists(atPath: lexicalModelPath) {
      log.error("Could not add lexical model with ID: \(lexicalModel.id) because the lexical model file does not exist")
      return
    }
    
    // Get lexical models list if it exists in user defaults, otherwise create a new one
    let userDefaults = Storage.active.userDefaults
    var userLexicalModels = userDefaults.userLexicalModels ?? []
    
    // Update lexical model if it exists
    if let index = userLexicalModels.index(where: { $0.fullID == lexicalModel.fullID }) {
      userLexicalModels[index] = lexicalModel
    } else {
      userLexicalModels.append(lexicalModel)
    }
    
    userDefaults.userLexicalModels = userLexicalModels
    userDefaults.set([Date()], forKey: Key.synchronizeSWLexicalModel)
    userDefaults.synchronize()
    log.info("Added lexical model ID: \(lexicalModel.id) name: \(lexicalModel.name)")
}

  /// Removes a keyboard from the list in the keyboard picker if it exists.
  /// - Returns: The keyboard exists and was removed
  public func removeKeyboard(withFullID fullID: FullKeyboardID) -> Bool {
    // Remove keyboard from the list if it exists
    let index = Storage.active.userDefaults.userKeyboards?.index { $0.fullID == fullID }
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
      let keyboardDir = Storage.active.keyboardDir(forID: kb.id)
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
    let index = Storage.active.userDefaults.userLexicalModels?.index { $0.fullID == fullID }
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
      let lexicalModelDir = Storage.active.lexicalModelDir(forID: lm.id)
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
      let index = userKeyboards.index(where: { self.currentKeyboardID == $0.fullID })
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
    let kb = Storage.active.userDefaults.userKeyboard(withFullID: fullID)
    if let filename = kb?.font?.source.first(where: { $0.hasFontExtension }) {
      let fontURL = Storage.active.fontURL(forKeyboardID: fullID.keyboardID, filename: filename)
      return FontManager.shared.fontName(at: fontURL)
    }
    return nil
  }

  /// - Returns: the OSK font name for the given keyboard ID and languageID, or returns nil if
  ///   - The keyboard doesn't have an OSK font
  ///   - The keyboard info is not available in the user keyboards list
  func oskFontNameForKeyboard(withFullID fullID: FullKeyboardID) -> String? {
    let kb = Storage.active.userDefaults.userKeyboard(withFullID: fullID)
    if let filename = kb?.oskFont?.source.first(where: { $0.hasFontExtension }) {
      let fontURL = Storage.active.fontURL(forKeyboardID: fullID.keyboardID, filename: filename)
      return FontManager.shared.fontName(at: fontURL)
    }
    return nil
  }
    
  // MARK: - Adhoc keyboards
  public func parseKbdKMP(_ folder: URL) throws -> Void {
    do {
      var path = folder
      path.appendPathComponent("kmp.json")
      let data = try Data(contentsOf: path, options: .mappedIfSafe)
      let jsonResult = try JSONSerialization.jsonObject(with: data, options: .mutableLeaves)
      if let jsonResult = jsonResult as? [String:AnyObject] {
        if let keyboards = jsonResult["keyboards"] as? [[String:AnyObject]] {
          for k in keyboards {
            let name = k["name"] as! String
            let keyboardID = k["id"] as! String
            let version = k["version"] as! String
            //true if the keyboard targets a right-to-left script. false if absent.
            let isrtl: Bool =  k["rtl"] as? Bool ?? false

            var oskFont: Font?
            let osk = k["oskFont"] as? String
            if let _ = osk {
              oskFont = Font(filename: osk!)
            }
            var displayFont: Font?
            let font = k["displayFont"] as? String
            if let _ = font {
              displayFont = Font(filename: font!)
            }
            
            //TODO: handle errors if languages do not exist
            var languageName = ""
            var languageId = ""
            
            var installableKeyboards : [InstallableKeyboard] = []
            if let langs = k["languages"] as? [[String:String]] {
              for l in langs {
                languageName = l["name"]!
                languageId = l["id"]!
                
                installableKeyboards.append( InstallableKeyboard(
                  id: keyboardID,
                  name: name,
                  languageID: languageId,
                  languageName: languageName,
                  version: version,
                  isRTL: isrtl,
                  font: displayFont,
                  oskFont: oskFont,
                  isCustom: false))
              }
            }
            
            do {
              try FileManager.default.createDirectory(at: Storage.active.keyboardDir(forID: keyboardID),
                                                      withIntermediateDirectories: true)
            } catch {
              log.error("Could not create dir for download: \(error)")
              throw KMPError.fileSystem
            }
            
            var haveInstalledOne = false
            for keyboard in installableKeyboards {
              let storedPath = Storage.active.keyboardURL(for: keyboard)
              
              var installableFiles: [[Any]] = [["\(keyboardID).js", storedPath]]
              if let osk = osk {
                let oskPath = Storage.active.fontURL(forKeyboardID: keyboardID, filename: osk)
                installableFiles.append([osk, oskPath])
              }
              
              if let font = font {
                let displayPath = Storage.active.fontURL(forKeyboardID: keyboardID, filename: font)
                installableFiles.append([font, displayPath])
              }
              do {
                for item in installableFiles {
                  var filePath = folder
                  if(FileManager.default.fileExists(atPath: (item[1] as! URL).path)) {
                    try FileManager.default.removeItem(at: item[1] as! URL)
                  }
                  filePath.appendPathComponent(item[0] as! String)
                  try FileManager.default.copyItem(at: filePath,
                                                   to: item[1] as! URL)
                  
                }
              } catch {
                log.error("Error saving the download: \(error)")
                throw KMPError.copyFiles
              }
              if !haveInstalledOne {
                Manager.shared.addKeyboard(keyboard)
                haveInstalledOne = true
              }
            }
          }
        }
      }
    } catch {
      log.error("error parsing keyboard kmp: \(error)")
      throw KMPError.invalidPackage
    }
  }
    
  // MARK: - Adhoc lexical models
  public func parseLMKMP(_ folder: URL) throws -> Void {
    do {
      var path = folder
      path.appendPathComponent("kmp.json")
      let data = try Data(contentsOf: path, options: .mappedIfSafe)
      let jsonResult = try JSONSerialization.jsonObject(with: data, options: .mutableLeaves)
      if let jsonResult = jsonResult as? [String:AnyObject] {
        if let lexicalModels = jsonResult["lexicalModels"] as? [[String:AnyObject]] {
          for k in lexicalModels {
            let name = k["name"] as! String
            let lexicalModelID = k["id"] as! String
            let version = k["version"] as! String
            
            //TODO: handle errors if languages do not exist
            //var languageName = ""
            var languageId = ""
            
            var installableLexicalModels : [InstallableLexicalModel] = []
            if let langs = k["languages"] as? [[String:String]] {
              for l in langs {
                //languageName = l["name"]!
                languageId = l["id"]!
                
                installableLexicalModels.append( InstallableLexicalModel(
                  id: lexicalModelID,
                  name: name,
                  languageID: languageId,
//                  languageName: languageName,
                  version: version,
                  isCustom: false))
              }
            }
            
            do {
              try FileManager.default.createDirectory(at: Storage.active.lexicalModelDir(forID: lexicalModelID),
                                                      withIntermediateDirectories: true)
            } catch {
              log.error("Could not create dir for download: \(error)")
              throw KMPError.fileSystem
            }
            
            for lexicalModel in installableLexicalModels {
              let storedPath = Storage.active.lexicalModelURL(for: lexicalModel)
              
              let installableFiles: [[Any]] = [["\(lexicalModelID).model.js", storedPath]]
              do {
                for item in installableFiles {
                  var filePath = folder
                  if(FileManager.default.fileExists(atPath: (item[1] as! URL).path)) {
                    try FileManager.default.removeItem(at: item[1] as! URL)
                  }
                  filePath.appendPathComponent(item[0] as! String)
                  try FileManager.default.copyItem(at: filePath,
                                                   to: item[1] as! URL)
                  
                }
              } catch {
                log.error("Error saving the lexical model download: \(error)")
                throw KMPError.copyFiles
              }
              Manager.shared.addLexicalModel(lexicalModel)
            }
          }
        }
      }
    } catch {
      log.error("error parsing lexical model kmp: \(error)")
      throw KMPError.invalidPackage
    }
  }
  
  public func unzipFile(fileUrl: URL, destination: URL, complete: @escaping () -> Void)
  {
    do {
      try Zip.unzipFile(fileUrl, destination: destination, overwrite: true,
                        password: nil,
                        progress: { (progress) -> () in
                          //TODO: add timeout
                          if(progress == 1.0) {
                            complete()
                          }
                        })
    } catch {
      log.error("error unzipping archive: \(error)")
    }
  }

  // MARK: - Downloading keyboards and lexical models

  /// Asynchronously fetches the .js file for the keyboard with given IDs.
  /// See `Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the keyboard files on failure
  ///   - fetchRepositoryIfNeeded: Fetch the list of keyboards from the API if necessary.
  public func downloadKeyboard(withID keyboardID: String,
                               languageID: String,
                               isUpdate: Bool,
                               fetchRepositoryIfNeeded: Bool = true) {
    guard let keyboards = apiKeyboardRepository.keyboards,
      let options = apiKeyboardRepository.options
    else {
      if fetchRepositoryIfNeeded {
        log.info("Fetching repository from API for keyboard download")
        apiKeyboardRepository.fetch { error in
          if let error = error {
            self.downloadFailed(forKeyboards: [], error: error)
          } else {
            log.info("Fetched repository. Continuing with keyboard download.")
            self.downloadKeyboard(withID: keyboardID,
                                  languageID: languageID,
                                  isUpdate: isUpdate,
                                  fetchRepositoryIfNeeded: false)
          }
        }
        return
      }
      let message = "Keyboard repository not yet fetched"
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
      downloadFailed(forKeyboards: [], error: error)
      return
    }

    guard let keyboard = apiKeyboardRepository.installableKeyboard(withID: keyboardID, languageID: languageID),
      let filename = keyboards[keyboardID]?.filename
    else {
      let message = "Keyboard not found with id: \(keyboardID), languageID: \(languageID)"
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: message])
      downloadFailed(forKeyboards: [], error: error)
      return
    }

    guard downloadQueue == nil else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forKeyboards: [keyboard], error: error)
      return
    }

    guard reachability.connection != Reachability.Connection.none else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: [keyboard], error: error)
      return
    }

    do {
      try FileManager.default.createDirectory(at: Storage.active.keyboardDir(forID: keyboardID),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      return
    }

    let keyboardURL = options.keyboardBaseURL.appendingPathComponent(filename)
    let fontURLs = Array(Set(keyboardFontURLs(forFont: keyboard.font, options: options) +
                             keyboardFontURLs(forFont: keyboard.oskFont, options: options)))

    // TODO: Better typing
    downloadQueue = HTTPDownloader(self)
    let commonUserData: [String: Any] = [
      Key.keyboardInfo: [keyboard],
      Key.update: isUpdate
    ]
    downloadQueue!.userInfo = commonUserData

    var request = HTTPDownloadRequest(url: keyboardURL, userInfo: commonUserData)
    request.destinationFile = Storage.active.keyboardURL(for: keyboard).path
    request.tag = 0
    downloadQueue!.addRequest(request)

    for (i, url) in fontURLs.enumerated() {
      request = HTTPDownloadRequest(url: url, userInfo: commonUserData)
      request.destinationFile = Storage.active.fontURL(forKeyboardID: keyboardID, filename: url.lastPathComponent).path
      request.tag = i + 1
      downloadQueue!.addRequest(request)
    }
    downloadQueue!.run()
    self.downloadLexicalModelsForLanguageIfExists(languageID: languageID)
  }

  private func keyboardFontURLs(forFont font: Font?, options: Options) -> [URL] {
    guard let font = font else {
      return []
    }
    return font.source.filter({ $0.hasFontExtension })
      .map({ options.fontBaseURL.appendingPathComponent($0) })
  }

  /// Downloads a custom keyboard from the URL
  /// - Parameters:
  ///   - url: URL to a JSON description of the keyboard
  public func downloadKeyboard(from url: URL) {
    guard reachability.connection != Reachability.Connection.none else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No connection"])
      downloadFailed(forKeyboards: [], error: error)
      return
    }

    guard let data = try? Data(contentsOf: url) else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Failed to fetch JSON file"])
      downloadFailed(forKeyboards: [], error: error)
      return
    }
    
    decodeKeyboardData(data, decodingStrategy: .ios8601WithFallback)
  }

  private func decodeKeyboardData(_ data: Data, decodingStrategy : JSONDecoder.DateDecodingStrategy) {
    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = decodingStrategy
  
    if let keyboard = try? decoder.decode(KeyboardAPICall.self, from: data) {
      downloadKeyboard(keyboard)
    } else {
      decoder.dateDecodingStrategy = .iso8601WithoutTimezone
      if let keyboard = try? decoder.decode(KeyboardAPICall.self, from: data) {
        downloadKeyboard(keyboard)
      } else {
        decoder.dateDecodingStrategy = .ios8601WithMilliseconds
        do {
          let keyboard = try decoder.decode(KeyboardAPICall.self, from: data)
          downloadKeyboard(keyboard)
        } catch {
          downloadFailed(forKeyboards: [], error: error)
        }
      }
    }
  }

  /// Assumes that Keyboard has font and oskFont set and ignores fonts contained in Language.
  private func downloadKeyboard(_ keyboardAPI: KeyboardAPICall) {
    let keyboard = keyboardAPI.keyboard
    let installableKeyboards = keyboard.languages!.map { language in
      InstallableKeyboard(keyboard: keyboard, language: language, isCustom: true)
    }

    let filename = keyboard.filename
    let keyboardURL = keyboardAPI.options.keyboardBaseURL.appendingPathComponent(filename)

    let fontURLs = Array(Set(keyboardFontURLs(forFont: keyboard.font, options: keyboardAPI.options) +
                             keyboardFontURLs(forFont: keyboard.oskFont, options: keyboardAPI.options)))

    if downloadQueue != nil {
      // Download queue is active.
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forKeyboards: installableKeyboards, error: error)
      return
    }

    if reachability.connection == Reachability.Connection.none {
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: installableKeyboards, error: error)
      return
    }

    do {
      try FileManager.default.createDirectory(at: Storage.active.keyboardDir(forID: keyboard.id),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      return
    }

    let isUpdate = Storage.active.userDefaults.userKeyboards?.contains { $0.id == keyboard.id } ?? false

    downloadQueue = HTTPDownloader(self)
    let commonUserData: [String: Any] = [
      Key.keyboardInfo: installableKeyboards,
      Key.update: isUpdate
    ]
    downloadQueue!.userInfo = commonUserData

    var request = HTTPDownloadRequest(url: keyboardURL, userInfo: commonUserData)
    request.destinationFile = Storage.active.keyboardURL(forID: keyboard.id, version: keyboard.version).path
    request.tag = 0

    downloadQueue!.addRequest(request)
    for (i, url) in fontURLs.enumerated() {
      request = HTTPDownloadRequest(url: url, userInfo: commonUserData)
      request.destinationFile = Storage.active.fontURL(forKeyboardID: keyboard.id, filename: url.lastPathComponent).path
      request.tag = i + 1
      downloadQueue!.addRequest(request)
    }
    downloadQueue!.run()
  }

  /// - Returns: The current state for a keyboard
  public func stateForKeyboard(withID keyboardID: String) -> KeyboardState {
    if keyboardIdForCurrentRequest() == keyboardID {
      return .downloading
    }
    let userKeyboards = Storage.active.userDefaults.userKeyboards
    guard let userKeyboard = userKeyboards?.first(where: { $0.id == keyboardID }) else {
      return .needsDownload
    }

    // Check version
    if let repositoryVersionString = apiKeyboardRepository.keyboards?[keyboardID]?.version {
      let downloadedVersion = Version(userKeyboard.version) ?? Version.fallback
      let repositoryVersion = Version(repositoryVersionString) ?? Version.fallback
      if downloadedVersion < repositoryVersion {
        return .needsUpdate
      }
    }
    return .upToDate
  }

  func keyboardIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasJavaScriptExtension {
        return String(tmpStr.dropLast(3))
      }
    } else if let downloadQueue = downloadQueue {
      let kbInfo = downloadQueue.userInfo[Key.keyboardInfo]
      if let keyboards = kbInfo as? [InstallableKeyboard], let keyboard = keyboards.first {
        return keyboard.id
      }
    }
    return nil
  }
  
  // return a lexical model so caller can use it in a downloadSucceeded call
  func  installLexicalModelPackage(downloadedPackageFile: URL) -> InstallableLexicalModel? {
    var installedLexicalModel: InstallableLexicalModel? = nil
    let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    var destination =  documentsDirectory
    destination.appendPathComponent("temp/\(downloadedPackageFile.lastPathComponent)")
    
    KeymanPackage.extract(fileUrl: downloadedPackageFile, destination: destination, complete: { kmp in
      if let kmp = kmp as! LexicalModelKeymanPackage? {
        do {
          try Manager.shared.parseLMKMP(kmp.sourceFolder)
          log.info("successfully parsed the lexical model in: \(kmp.sourceFolder)")
          installedLexicalModel = kmp.models[0].installableLexicalModels[0]
          //this can fail gracefully and not show errors to users
          try FileManager.default.removeItem(at: downloadedPackageFile)
        } catch {
          log.error("Error installing the lexical model: \(error)")
        }
      } else {
        log.error("Error extracting the lexical model from the package: \(KMPError.invalidPackage)")
      }
    })
    return installedLexicalModel
  }
  
  func downloadLexicalModelPackage(string lexicalModelPackageURLString: String) -> Void {
    if let lexicalModelKMPURL = URL.init(string: lexicalModelPackageURLString) {
      //determine where to put the data (local  file  URL)
      var destinationUrl = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
      destinationUrl.appendPathComponent("\(lexicalModelKMPURL.lastPathComponent).zip")
      //callback to handle the data downloaded
      func lexicalModelDownloaded(data: Data?,
                                  response: URLResponse?,
                                  dest: URL,
                                  error: Error?) {
        if let error = error {
          log.error("Failed to fetch lexical model KMP file")
          downloadFailed(forLexicalModelPackage: lexicalModelPackageURLString, error: error)
        } else {
          do {
            try data!.write(to: dest)
          } catch {
            log.error("Error writing the lexical model download data: \(error)")
          }
          if let lm = installLexicalModelPackage(downloadedPackageFile: dest) {
            downloadSucceeded(forLexicalModel: lm)
          } else {
            let installError = NSError(domain: "Keyman", code: 0,
                                       userInfo: [NSLocalizedDescriptionKey: "installError"])
            downloadFailed(forLexicalModelPackage: lexicalModelPackageURLString, error: installError )
          }
        }
      }

      log.info("downloading lexical model from Keyman cloud: \(lexicalModelKMPURL).")
      let task = URLSession.shared.dataTask(with: lexicalModelKMPURL) { (data, response, error) in
        DispatchQueue.main.async {
          lexicalModelDownloaded(data: data, response: response, dest: destinationUrl, error: error)
        }
      }
      task.resume()

    } else {
      log.info("\(lexicalModelPackageURLString) is not a URL string?")
      // might want to download the .js file directly, then, instead
    }
  }
  
  /// Starts the process of fetching the package file of the lexical model for the given language ID
  ///   first it fetches the list of lexical models for the given language
  ///   then it takes the first of the list and download the KMP package file and asks the app to open it (like adhoc download)
  /// - Parameters:
  ///   - languageID: the bcp47 string of the desired language
  public func downloadLexicalModelsForLanguageIfExists(languageID: String) {
    //get list of lexical models for this languageID  /?q=bcp47:en
    func listCompletionHandler(lexicalModels: [LexicalModel]?, error: Error?) -> Void {
      if let error = error {
        log.info("Failed to fetch lexical model list for "+languageID+". error: "+(error as! String))
        self.downloadFailed(forLanguageID: languageID, error: error) //???forKeyboards
      } else if nil == lexicalModels {
        //TODO: put up an alert instead
        log.info("No lexical models available for language \(languageID) (nil)")
      } else if 0 == lexicalModels?.count {
        log.info("No lexical models available for language \(languageID) (empty)")
      } else {
        log.info("Fetched lexical model list for "+languageID+".")
        // choose which of the lexical models to download
        //  for now, this just downloads the first one
        let chosenIndex = 0
        if let lexicalModel = lexicalModels?[chosenIndex] {
          downloadLexicalModelPackage(string: lexicalModel.packageFilename)
        } else {
          log.info("no error, but no lexical model in list, either!")
        }
      }
    }
    
    apiLexicalModelRepository.fetchList(languageID: languageID, completionHandler: listCompletionHandler)
  }

  /// Asynchronously fetches the .js file for the lexical model with given IDs.
  /// See `Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the lexical model files on failure
  ///   - fetchRepositoryIfNeeded: Fetch the list of lexical models from the API if necessary.
  public func downloadLexicalModel(withID lexicalModelID: String,
                                   languageID: String,
                                   isUpdate: Bool,
                                   fetchRepositoryIfNeeded: Bool = true) {
//    guard let lexicalModels = apiLexicalModelRepository.lexicalModels,
//      else {
//        if fetchRepositoryIfNeeded {
//          log.info("Fetching repository from API for lexical model download")
//          apiLexicalModelRepository.fetch { error in
//            if let error = error {
//              self.downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
//            } else {
//              log.info("Fetched repository. Continuing with lexical model download.")
//              self.downloadLexicalModel(withID: lexicalModelID,
//                                        languageID: languageID,
//                                        isUpdate: isUpdate,
//                                        fetchRepositoryIfNeeded: false)
//            }
//          }
//          return
//        }
//        let message = "Lexical model repository not yet fetched"
//        let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
//        downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
//        return

//    guard let lexicalModel = apiLexicalModelRepository.installableLexicalModel(withID: lexicalModelID, languageID: languageID),
//      let filename = lexicalModels[lexicalModelID]?.filename
//      else {
//        let message = "Lexical model not found with id: \(lexicalModelID), languageID: \(languageID)"
//        let error = NSError(domain: "Keyman", code: 0,
//                            userInfo: [NSLocalizedDescriptionKey: message])
//        downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
//        return
//    }
//
//    guard downloadQueue == nil else {
//      let error = NSError(domain: "Keyman", code: 0,
//                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
//      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels : [lexicalModel]
//      return
//    }
//
//    guard reachability.connection != Reachability.Connection.none else {
//      let error = NSError(domain: "Keyman", code: 0,
//                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
//      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels : [lexicalModel]
//      return
//    }
//
//    do {
//      try FileManager.default.createDirectory(at: Storage.active.lexicalModelDir(forID: lexicalModelID),
//                                              withIntermediateDirectories: true)
//    } catch {
//      log.error("Could not create dir for download: \(error)")
//      return
//    }
//
//    let lexicalModelURL = lexicalModelBaseURL?.appendingPathComponent(filename)
//
//    // TODO: Better typing
//    downloadQueue = HTTPDownloader(self)
//    let commonUserData: [String: Any] = [
//      Key.lexicalModelInfo: [lexicalModel],
//      Key.update: isUpdate
//    ]
//    downloadQueue!.userInfo = commonUserData
//
//    let request = HTTPDownloadRequest(url: lexicalModelURL!, userInfo: commonUserData)
//    request.destinationFile = Storage.active.lexicalModelURL(for: lexicalModel).path
//    request.tag = 0
//    downloadQueue!.addRequest(request)
//
//    downloadQueue!.run()
  }
  
  private func lexicalModelFontURLs(forFont font: Font?, options: Options) -> [URL] {
    guard let font = font else {
      return []
    }
    return font.source.filter({ $0.hasFontExtension })
      .map({ options.fontBaseURL.appendingPathComponent($0) })
  }
  
  /// Downloads a custom lexical model from the URL
  /// - Parameters:
  ///   - url: URL to a JSON description of the lexical model
  public func downloadLexicalModel(from url: URL) {
    guard reachability.connection != Reachability.Connection.none else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No connection"])
      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
      return
    }
    
    guard let data = try? Data(contentsOf: url) else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Failed to fetch JSON file"])
      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
      return
    }
    
    decodeLexicalModelData(data, decodingStrategy: .ios8601WithFallback)
  }
  
  private func decodeLexicalModelData(_ data: Data, decodingStrategy : JSONDecoder.DateDecodingStrategy) {
    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = decodingStrategy
    
    if let lexicalModel = try? decoder.decode(LexicalModelAPICall.self, from: data) {
      downloadLexicalModel(lexicalModel)
    } else {
      decoder.dateDecodingStrategy = .iso8601WithoutTimezone
      if let lexicalModel = try? decoder.decode(LexicalModelAPICall.self, from: data) {
        downloadLexicalModel(lexicalModel)
      } else {
        decoder.dateDecodingStrategy = .ios8601WithMilliseconds
        do {
          let lexicalModel = try decoder.decode(LexicalModelAPICall.self, from: data)
          downloadLexicalModel(lexicalModel)
        } catch {
          downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels
        }
      }
    }
  }
  
  private func downloadLexicalModel(_ lexicalModelAPI: LexicalModelAPICall) {
    let lexicalModel = lexicalModelAPI.lexicalModels[0]
    let installableLexicalModels = lexicalModel.languages.map { language in
      InstallableLexicalModel(lexicalModel: lexicalModel, languageID: language, isCustom: true)
    }
    
    let packageFilename = lexicalModel.packageFilename
    let lexicalModelURL = URL(string: "https://api.keyman.com/model")!.appendingPathComponent(packageFilename)
    
    if downloadQueue != nil {
      // Download queue is active.
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels : installableLexicalModels
      return
    }
    
    if reachability.connection == Reachability.Connection.none {
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: [], error: error) //??? forLexicalModels : installableLexicalModels
      return
    }
    
    do {
      try FileManager.default.createDirectory(at: Storage.active.lexicalModelDir(forID: lexicalModel.id),
                                              withIntermediateDirectories: true)
    } catch {
      log.error("Could not create dir for download: \(error)")
      return
    }
    
    let isUpdate = Storage.active.userDefaults.userLexicalModels?.contains { $0.id == lexicalModel.id } ?? false
    
    downloadQueue = HTTPDownloader(self)
    let commonUserData: [String: Any] = [
      Key.lexicalModelInfo: installableLexicalModels,
      Key.update: isUpdate
    ]
    downloadQueue!.userInfo = commonUserData
    
    let request = HTTPDownloadRequest(url: lexicalModelURL, userInfo: commonUserData)
    request.destinationFile = Storage.active.lexicalModelURL(forID: lexicalModel.id, version: lexicalModel.version ?? InstallableConstants.defaultVersion).path
    request.tag = 0
    
    downloadQueue!.addRequest(request)
    downloadQueue!.run()
  }
  
  /// - Returns: The current state for a lexical model
  //TODO: rename KeyboardState to ResourceState? so it can be used with both keybaoards and lexical models without confusion
  public func stateForLexicalModel(withID lexicalModelID: String) -> KeyboardState {
    if lexicalModelIdForCurrentRequest() == lexicalModelID {
      return .downloading
    }
    let userLexicalModels = Storage.active.userDefaults.userLexicalModels
    guard let userLexicalModel = userLexicalModels?.first(where: { $0.id == lexicalModelID }) else {
      return .needsDownload
    }
    
    // Check version
    if let repositoryVersionString = apiLexicalModelRepository.lexicalModels?[lexicalModelID]?.version {
      let downloadedVersion = Version(userLexicalModel.version) ?? Version.fallback
      let repositoryVersion = Version(repositoryVersionString) ?? Version.fallback
      if downloadedVersion < repositoryVersion {
        return .needsUpdate
      }
    }
    return .upToDate
  }
  
  func lexicalModelIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasJavaScriptExtension {
        return String(tmpStr.dropLast(3))
      }
    } else if let downloadQueue = downloadQueue {
      let kbInfo = downloadQueue.userInfo[Key.lexicalModelInfo]
      if let lexicalModels = kbInfo as? [InstallableLexicalModel], let lexicalModel = lexicalModels.first {
        return lexicalModel.id
      }
    }
    return nil
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

  // MARK: - HTTPDownloadDelegate methods

  func downloadQueueFinished(_ queue: HTTPDownloader) { }

  func downloadRequestStarted(_ request: HTTPDownloadRequest) {
    // If we're downloading a new keyboard.
    // The extra check is there to filter out other potential request types in the future.
    if request.tag == 0 && request.typeCode == .downloadFile {
      NotificationCenter.default.post(name: Notifications.keyboardDownloadStarted,
                                      object: self,
                                      value: request.userInfo[Key.keyboardInfo] as! [InstallableKeyboard])
    }
  }

  func downloadRequestFinished(_ request: HTTPDownloadRequest) {
    switch request.typeCode {
    case .downloadFile:
      let keyboards = request.userInfo[Key.keyboardInfo] as! [InstallableKeyboard]
      let keyboard = keyboards[0]
      let isUpdate = request.userInfo[Key.update] as! Bool

      if let statusCode = request.responseStatusCode, statusCode == 200 {
        // The request has succeeded.
        if downloadQueue!.requestsCount == 0 {
          // Download queue finished.
          downloadQueue = nil
          FontManager.shared.registerCustomFonts()
          log.info("Downloaded keyboard: \(keyboard.id).")

          NotificationCenter.default.post(name: Notifications.keyboardDownloadCompleted,
                                          object: self,
                                          value: keyboards)
          if isUpdate {
            shouldReloadKeyboard = true
            inputViewController.reload()
          }
          let userDefaults = Storage.active.userDefaults
          userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
          userDefaults.synchronize()
        }
      } else { // Possible request error (400 Bad Request, 404 Not Found, etc.)
        downloadQueue!.cancelAllOperations()
        downloadQueue = nil

        let errorMessage = "\(request.responseStatusMessage ?? ""): \(request.url)"
        let error = NSError(domain: "Keyman", code: 0,
                            userInfo: [NSLocalizedDescriptionKey: errorMessage])
        log.error("Keyboard download failed: \(error).")

        if !isUpdate {
          // Clean up keyboard file if anything fails
          // TODO: Also clean up remaining fonts
          try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboard))
        }
        downloadFailed(forKeyboards: keyboards, error: error)
      }
    }
  }

  func downloadRequestFailed(_ request: HTTPDownloadRequest) {
    switch request.typeCode {
    case .downloadFile:
      downloadQueue = nil
      let error = request.error!
      log.error("Keyboard download failed: \(error).")

      let keyboards = request.userInfo[Key.keyboardInfo] as! [InstallableKeyboard]
      let keyboard = keyboards[0]
      let isUpdate = request.userInfo[Key.update] as! Bool

      if !isUpdate {
        // Clean up keyboard file if anything fails
        // TODO: Also clean up remaining fonts
        try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboard))
      }
      downloadFailed(forKeyboards: keyboards, error: error as NSError)
    }
  }

  private func downloadFailed(forKeyboards keyboards: [InstallableKeyboard], error: Error) {
    let notification = KeyboardDownloadFailedNotification(keyboards: keyboards, error: error)
    NotificationCenter.default.post(name: Notifications.keyboardDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  private func downloadFailed(forLanguageID languageID: String, error: Error) {
    let notification = LexicalModelDownloadFailedNotification(lmOrLanguageID: languageID, error: error)
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  private func downloadFailed(forLexicalModelPackage packageURL: String, error: Error) {
    let notification = LexicalModelDownloadFailedNotification(lmOrLanguageID: packageURL, error: error)
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadFailed,
                                    object: self,
                                    value: notification)
  }
  
  private func downloadSucceeded(forLexicalModel lm: InstallableLexicalModel) {
    let notification = LexicalModelDownloadCompletedNotification([lm])
    NotificationCenter.default.post(name: Notifications.lexicalModelDownloadCompleted,
                                    object: self,
                                    value: notification)
  }

  // MARK: - Loading custom keyboards
  /// Preloads the JS and font files required for a keyboard.
  public func preloadFiles(forKeyboardID keyboardID: String, at urls: [URL], shouldOverwrite: Bool) throws {
    let keyboardDir = Storage.active.keyboardDir(forID: keyboardID)
    try FileManager.default.createDirectory(at: keyboardDir, withIntermediateDirectories: true)
    for url in urls {
      try Storage.copyAndExcludeFromBackup(at: url,
                                           to: keyboardDir.appendingPathComponent(url.lastPathComponent),
                                           shouldOverwrite: shouldOverwrite)
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
    let vc = shouldAddKeyboard ? KeyboardPickerViewController() :
      KeyboardSwitcherViewController()
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
    let device = Device()

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
}
