//
//  Manager.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import CoreText
import UIKit
import WebKit

typealias FetchKeyboardsBlock = ([String: Any]?) -> Void

// MARK: - Constants

// Possible states that a keyboard can be in
public enum KeyboardState {
  case needsDownload
  case needsUpdate
  case upToDate
  case downloading
  case none
}

// TODO: Use a struct

// Strings
private let keyboardChangeHelpText = "Tap here to change keyboard"

// URLs
private let apiBaseURL = "https://r.keymanweb.com/api/4.0/"
private let apiRemoteURL = "https://r.keymanweb.com/api/2.0/remote?url="
private let keymanHostName = "r.keymanweb.com"

// UI In-App Keyboard Constants
private let phonePortraitInAppKeyboardHeight: CGFloat = 183.0
private let phoneLandscapeInAppKeyboardHeight: CGFloat = 183.0
private let padPortraitInAppKeyboardHeight: CGFloat = 385.0
private let padLandscapeInAppKeyboardHeight: CGFloat = 385.0

// UI System Keyboard Constants
private let phonePortraitSystemKeyboardHeight: CGFloat = 216.0
private let phoneLandscapeSystemKeyboardHeight: CGFloat = 162.0
private let padPortraitSystemKeyboardHeight: CGFloat = 264.0
private let padLandscapeSystemKeyboardHeight: CGFloat = 352.0

public class Manager: NSObject, WKNavigationDelegate, WKScriptMessageHandler, HTTPDownloadDelegate,
UIGestureRecognizerDelegate {
  /// Application group identifier for shared container. Set this before accessing the shared manager.
  public static var applicationGroupIdentifier: String?

  public static let shared = Manager()

  public var isDebugPrintingOn = false

  /// Display the help bubble on first use.
  public var isKeymanHelpOn = true

  // TODO: Change API to not disable removing as well
  /// Allow users to add new keyboards in the keyboard picker.
  ///  - Default value is true.
  ///  - Setting this to false will also disable keyboard removal. To enable keyboard removal you should set
  ///    canRemoveKeyboards to true.
  ///  - If set to false, calling fetchKeyboardList() is unnecessary and should be avoided unless you want to use auto
  ///    keyboard update check feature of the keyboard picker.
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

  // TODO: Use a struct instead of dictionaries with fixed keys
  /// The list of Keyman languages once they have been fetched.
  /// - Each language is an NSDictionary with a name, id, and a list of keyboards
  /// - Each keyboard is itself an NSDictionary with id, name, etc
  /// - All you should ever require from this list are names and IDs
  /// - If you find yourself using other info (like the URI), there is probably a Manager method that does what you
  ///   want already
  /// - This list won't be available until .languagesUpdated has been broadcasted
  public private(set) var languages: [Language] = []

  /// Dictionary of Keyman keyboards to store language and keyboard names etc.
  ///
  /// The key format is $languageID_$keyboardID. For example, "eng_european2" returns the English EuroLatin2 keyboard
  public private(set) var keyboardsDictionary: [String: InstallableKeyboard] = [:]

  /// Dictionary of available Keyman keyboard fonts keyed by font filename
  public private(set) var keymanFonts: [String: RegisteredFont] = [:]

  /// Keyman system-wide keyboard
  public let isSystemKeyboard: Bool

  /// The version of the Keyman SDK
  public var sdkVersion: String {
    let info = NSDictionary(contentsOfFile: keymanBundle.path(forResource: "KeymanEngine-Info",
                                                              ofType: "plist")!)
    return info!["CFBundleVersion"] as! String
  }

  /// Keyman Web resources
  public var keymanBundle: Bundle {
    return Bundle(path: Bundle(for: Manager.self).path(forResource: "Keyman", ofType: "bundle")!)!
  }

  /// In keyboard extensions (system keyboard), `UIApplication.openURL(_:)` is unavailable. The API is not called in
  /// the system keyboard since `KeyboardInfoViewController` is never used. `openURL(:_)` is only used in applications,
  /// where it is safe. However, the entire Keyman Engine framework must be compiled with extension-safe APIs.
  ///
  /// Set this to `UIApplication.shared.openURL` in your application.
  public var openURL: ((URL) -> Bool)?

  var keyboardID: String?
  var languageID: String?
  weak var webDelegate: KeymanWebViewDelegate?
  weak var inputDelegate: KeymanWebViewDelegate?
  var currentRequest: HTTPDownloadRequest?
  var keyboardsInfo: [String: Keyboard]?
  var shouldReloadKeyboard = false
  var inputView: WKWebView! = nil

  private var downloadQueue: HTTPDownloader?
  private var sharedQueue: HTTPDownloader!
  private var reachability: Reachability!
  private var didSynchronize = false
  private var didResizeToOrientation = false
  private var lastKeyboardSize: CGSize = .zero
  private var specialOSKFont: String?

  private let subKeyColor = #colorLiteral(red: 244.0 / 255.0, green: 244.0 / 255.0, blue: 244.0 / 255.0, alpha: 1.0)
  private let subKeyColorHighlighted = #colorLiteral(red: 136.0 / 255.0, green: 136.0 / 255.0, blue: 1.0, alpha: 1.0)

  // Views
  private var helpBubbleView: PopoverView?
  private var keyPreviewView: KeyPreviewView?
  private var subKeysView: SubKeysView?
  private var keyboardMenuView: KeyboardMenuView?

  // Arrays
  private var subKeyIDs: [String] = []
  private var subKeyTexts: [String] = []
  private var subKeys: [UIButton] = []

  // Key frames
  private var keyFrame = CGRect.zero
  private var menuKeyFrame = CGRect.zero

  // Dictionary of Keyman options
  var options: Options?

  // MARK: - Object Admin
  deinit {
    NotificationCenter.default.removeObserver(self)
    // FIXME: Likely unneeded unless a reference exists to currentRequest outside of Manager
    if let currentRequest = currentRequest {
      currentRequest.userInfo["completionBlock"] = nil
    }
  }

  private override init() {
    let infoDict = Bundle.main.infoDictionary
    let extensionInfo = infoDict?["NSExtension"] as? [AnyHashable: Any]
    let extensionID = extensionInfo?["NSExtensionPointIdentifier"] as? String
    isSystemKeyboard = extensionID == "com.apple.keyboard-service"

    super.init()

    URLProtocol.registerClass(KeymanURLProtocol.self)

    if !isSystemKeyboard {
      copyUserDefaultsToSharedContainer()
      if let shared = Storage.shared,
        let nonShared = Storage.nonShared {
        do {
          try nonShared.copyFiles(to: shared)
        } catch {
          kmLog("Failed to copy files to shared container: \(error)", checkDebugPrinting: false)
        }
      }
      let userData = Storage.active.userDefaults
      let isKPDisplayed = userData.bool(forKey: Key.keyboardPickerDisplayed)
      if isKPDisplayed {
        isKeymanHelpOn = false
      }
    } else {
      isKeymanHelpOn = false
    }

    do {
      try Storage.active.copyKMWFiles(from: keymanBundle)
    } catch {
      kmLog("Failed to copy KMW files from bundle: \(error)", checkDebugPrinting: false)
    }

    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillShow),
                                           name: .UIKeyboardWillShow, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillHide),
                                           name: .UIKeyboardWillHide, object: nil)

    let kbVersion = latestKeyboardFileVersion(withID: Constants.defaultKeyboard.id)
    updateKeyboardVersion(forID: Constants.defaultKeyboard.id, newKeyboardVersion: kbVersion!)

    inputView = createInputView() // Pre-load keyboard

    // Set UILongPressGestureRecognizer to show sub keys
    let hold = UILongPressGestureRecognizer(target: self, action: #selector(self.holdAction))
    hold.minimumPressDuration = 0.5
    hold.delegate = self
    inputView.addGestureRecognizer(hold)

    reachability = Reachability(hostName: keymanHostName)
    NotificationCenter.default.addObserver(self, selector: #selector(self.reachabilityChanged),
                                           name: .reachabilityChanged, object: reachability)
    reachability.startNotifier()

    /* HTTPDownloader only uses this for its delegate methods.  So long as we don't
     * set the queue running, this should be perfectly fine.
     */
    sharedQueue = HTTPDownloader.init(self)
    registerCustomFonts()
  }

  // MARK: - Keyboard management

  /// Sets the current keyboard, querying from the user's list of keyboards.
  ///
  /// - Precondition:
  ///   - The keyboard must be added with `addKeyboard()`.
  ///
  /// - SeeAlso:
  ///   - addKeyboard()
  /// - Returns: Whether the keyboard was set successfully
  public func setKeyboard(withID keyboardID: String, languageID: String) -> Bool {
    if let keyboard = Storage.active.userDefaults.userKeyboard(withID: keyboardID, languageID: languageID) {
      return setKeyboard(keyboard)
    }
    return false
  }

  /// Set the current keyboard.
  ///
  /// - Returns: Whether the keyboard was set successfully
  public func setKeyboard(_ kb: InstallableKeyboard) -> Bool {
    if kb.languageID == self.languageID && kb.id == self.keyboardID {
      kmLog("Keyboard unchanged: \(kb.languageID)_\(kb.id)", checkDebugPrinting: true)
      return false
    }

    kmLog("Setting language: \(kb.languageID)_\(kb.id)", checkDebugPrinting: true)

    // FIXME: kb.version is not respected. Ideally we should be able to trust that the version number in UserDefaults
    // is-to-date but it is sometimes not updated.
    guard let kbVersion = latestKeyboardFileVersion(withID: kb.id) else {
      kmLog("Could not set keyboardID to \(kb.id) because the keyboard file does not exist",
        checkDebugPrinting: false)
      // Fallback to default keyboard if no keyboard is currently set.
      if (self.keyboardID == nil || self.languageID == nil) && kb.id != Constants.defaultKeyboard.id {
        _ = setKeyboard(Constants.defaultKeyboard)
      }
      return false
    }

    self.languageID = kb.languageID
    self.keyboardID = kb.id

    let jsFont = self.jsFont(fromFont: kb.font) ?? "undefined"
    let jsOskFont: String
    if let oskFont = kb.oskFont {
      jsOskFont = self.jsFont(fromFont: oskFont) ?? "undefined"
    } else {
      jsOskFont = jsFont
    }

    let escapedLangName = kb.languageName.replacingOccurrences(of: "'", with: "\\'")
    let escapedKbName = kb.name.replacingOccurrences(of: "'", with: "\\'")
    let jsString = """
      setKeymanLanguage('\(escapedKbName)','\(kb.id)','\(escapedLangName)',\
      '\(kb.languageID)','\(kbVersion)',\(jsFont),\(jsOskFont))
      """
    kmLog("Evaluating JavaScript: \(jsString)", checkDebugPrinting: true)
    inputView.evaluateJavaScript(jsString, completionHandler: nil)

    let userData = isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults

    userData.currentKeyboard = kb
    userData.synchronize()

    if isKeymanHelpOn {
      helpBubbleView?.removeFromSuperview()
      let showHelpBubble = #selector(self.showHelpBubble as () -> Void)
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: showHelpBubble, object: nil)
      perform(showHelpBubble, with: nil, afterDelay: 1.5)
    }

    NotificationCenter.default.post(name: Notifications.keyboardChanged,
                                    object: self,
                                    value: kb)
    return true
  }

  /// Adds a new keyboard to the list in the keyboard picker if it doesn't already exist.
  /// The keyboard must be downloaded (see `downloadKeyboard()`) or preloaded (see `preloadLanguageFile()`)
  ///
  /// - Parameters:
  ///   - isRTL: The writing direction is right to left
  ///   - isCustom: The keyboard is not provided by Keyman
  ///   - font: Custom font for text views as a JSON String (see keyboardsDictionary)
  ///   - oskFont: Font for the on-screen keyboard
  public func addKeyboard(_ keyboard: InstallableKeyboard) {
    let keyboardPath = Storage.active.keyboardURL(for: keyboard).path
    if !FileManager.default.fileExists(atPath: keyboardPath) {
      kmLog("Could not add keyboard with ID: \(keyboard.id) because the keyboard file does not exist",
        checkDebugPrinting: false)
      return
    }

    // Get keyboards list if it exists in user defaults, otherwise create a new one
    let userDefaults = Storage.active.userDefaults
    var userKeyboards = userDefaults.userKeyboards ?? []

    // Update keyboard if it exists
    if let index = userKeyboards.index(where: { $0.id == keyboard.id && $0.languageID == keyboard.languageID }) {
      userKeyboards[index] = keyboard
    } else {
      userKeyboards.append(keyboard)
    }

    userDefaults.userKeyboards = userKeyboards
    userDefaults.set([Date()], forKey: Key.synchronizeSWKeyboard)
    userDefaults.synchronize()
  }

  /// Removes a keyboard from the list in the keyboard picker if it exists.
  /// - Returns: The keyboard exists and was removed
  public func removeKeyboard(withID keyboardID: String, languageID: String) -> Bool {
    // Remove keyboard from the list if it exists
    let index = Storage.active.userDefaults.userKeyboards?.index { $0.id == keyboardID && $0.languageID == languageID }
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

    // Set a new keyboard if deleting the current one
    if kb.id == keyboardID && kb.languageID == languageID {
      setKeyboard(userKeyboards[0])
    }

    NotificationCenter.default.post(name: Notifications.keyboardRemoved, object: self, value: kb)
    return true
  }

  public func repositoryKeyboard(withID keyboardID: String, languageID: String) -> InstallableKeyboard? {
    return keyboardsDictionary["\(languageID)_\(keyboardID)"]
  }

  /// - Returns: Info for the current keyboard, if a keyboard is set
  public var currentKeyboardInfo: InstallableKeyboard? {
    guard let keyboardID = keyboardID, let languageID = languageID else {
      return nil
    }
    return Storage.active.userDefaults.userKeyboard(withID: keyboardID, languageID: languageID)
  }

  /// Switch to the next keyboard.
  /// - Returns: Index of the newly selected keyboard.
  public func switchToNextKeyboard() -> Int? {
    guard let userKeyboards = Storage.active.userDefaults.userKeyboards,
          let index = userKeyboards.index(where: { isCurrentKeyboard($0) }) else {
      return nil
    }
    let newIndex = (index + 1) % userKeyboards.count
    setKeyboard(userKeyboards[newIndex])
    return newIndex
  }

  func isCurrentKeyboard(withID keyboardID: String?, languageID: String?) -> Bool {
    return self.keyboardID == keyboardID && self.languageID == languageID
  }

  func isCurrentKeyboard(_ keyboard: InstallableKeyboard) -> Bool {
    return keyboard.id == self.keyboardID && keyboard.languageID == self.languageID
  }

  /// - Returns: The font name for the given keyboard ID and languageID, or returns nil if
  ///   - The keyboard doesn't have a font
  ///   - The keyboard info is not available in the user keyboards list or in keyboardsDictionary
  public func fontNameForKeyboard(withID keyboardID: String, languageID: String) -> String? {
    let kb = Storage.active.userDefaults.userKeyboard(withID: keyboardID, languageID: languageID)
      ?? repositoryKeyboard(withID: keyboardID, languageID: languageID)
    if let filename =  kb?.font?.source.first(where: { $0.hasFontExtension }) {
      return keymanFonts[filename]?.name
    }
    return nil
  }

  /// - Returns: the OSK font name for the given keyboard ID and languageID, or returns nil if
  ///   - The keyboard doesn't have an OSK font
  ///   - The keyboard info is not available in the user keyboards list or in keyboardsDictionary
  func oskFontNameForKeyboard(withID keyboardID: String, languageID: String) -> String? {
    let kb = Storage.active.userDefaults.userKeyboard(withID: keyboardID, languageID: languageID)
      ?? repositoryKeyboard(withID: keyboardID, languageID: languageID)
    if let filename =  kb?.oskFont?.source.first(where: { $0.hasFontExtension }) {
      return keymanFonts[filename]?.name
    }
    return nil
  }

  func isRTLKeyboard(withID keyboardID: String, languageID: String) -> Bool? {
    let kb = Storage.active.userDefaults.userKeyboard(withID: keyboardID, languageID: languageID)
      ?? repositoryKeyboard(withID: keyboardID, languageID: languageID)
    return kb?.isRTL
  }

  func jsFont(fromFont font: Font?) -> String? {
    guard let font = font else {
      return jsFont(fromFontDictionary: nil)
    }
    return jsFont(fromFontDictionary: [
      Key.fontFamily: font.family,
      Key.fontSource: font.source,
      "size": font.size
    ])
  }

  func jsFont(fromFontDictionary fontDict: [AnyHashable: Any]?) -> String? {
    guard let fontDict = fontDict, !fontDict.isEmpty else {
      return nil
    }

    let data: Data
    do {
      data = try JSONSerialization.data(withJSONObject: fontDict, options: [])
    } catch {
      kmLog("Failed to encode font dictionary as JSON: \(String(describing: fontDict))", checkDebugPrinting: false)
      return nil
    }

    return String(data: data, encoding: .ascii)!
      .replacingOccurrences(of: Key.fontFilename, with: Key.fontFiles)
      .replacingOccurrences(of: Key.fontSource, with: Key.fontFiles)
  }

  // MARK: - Downloading keyboards

  /// Asynchronously fetches the dictionary of possible languages/keyboards to be displayed in the keyboard picker.
  /// If not called before the picker is shown, the dictionary will be fetched automatically.
  /// This method allows you to fetch the info in advance at a time that's appropriate for your app.
  /// See `Notifications` for a list of relevant notifications.
  ///
  /// To save bandwidth, a cached version is used if:
  /// - the Keyman server is unreachable
  /// - the list has been recently fetched
  public func fetchKeyboardsList() {
    // TODO: Merge with this function
    fetchKeyboards(completionBlock: nil)
  }

  // This function appears to fetch the keyboard metadata from r.keymanweb.com.
  func fetchKeyboards(completionBlock: FetchKeyboardsBlock? = nil) {
    if currentRequest != nil {
      return
    }

    let deviceType = (UIDevice.current.userInterfaceIdiom == .phone) ? "iphone" : "ipad"
    let url = URL(string: "\(apiBaseURL)languages?dateformat=seconds&device=\(deviceType)")!
    let userData = completionBlock.map { ["completionBlock": $0] } ?? [:]

    let request = HTTPDownloadRequest(url: url, downloadType: .downloadCachedData, userInfo: userData)
    currentRequest = request
    sharedQueue.addRequest(request)
    sharedQueue.run()
  }

  /// Asynchronously fetches the .js file for the keyboard with given IDs.
  /// See `Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the keyboard files on failure
  public func downloadKeyboard(withID keyboardID: String, languageID: String, isUpdate: Bool) {
    guard let keyboardsInfo = keyboardsInfo else {
      let message = "Keyboard info has not yet been fetched. Call fetchKeyboardsList() first."
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
      downloadFailed(forKeyboards: [], error: error)
      return
    }

    guard let keyboard = repositoryKeyboard(withID: keyboardID, languageID: languageID) else {
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

    guard reachability.currentReachabilityStatus() != NotReachable else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: [keyboard], error: error)
      return
    }

    let filename = keyboardsInfo[keyboardID]!.filename
    let keyboardURL = options!.keyboardBaseURL.appendingPathComponent(filename)

    let fontURLs = Array(Set(keyboardFontURLs(forFont: keyboard.font, options: options!) +
                             keyboardFontURLs(forFont: keyboard.oskFont, options: options!)))

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
      request.destinationFile = Storage.active.fontURL(forFilename: url.lastPathComponent).path
      request.tag = i + 1
      downloadQueue!.addRequest(request)
    }
    downloadQueue!.run()
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
    guard reachability.currentReachabilityStatus() != NotReachable else {
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

    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = .ios8601WithFallback
    do {
      let keyboard = try decoder.decode(KeyboardAPICall.self, from: data)
      return downloadKeyboard(keyboard)
    } catch {
      downloadFailed(forKeyboards: [], error: error)
      return
    }
  }

  /// Assumes that Keyboard has font and oskFont set and ignores fonts contained in Language.
  private func downloadKeyboard(_ keyboardAPI: KeyboardAPICall) {
    let keyboard = keyboardAPI.keyboard
    let installableKeyboards = keyboard.languages!.map { language in
      InstallableKeyboard(keyboard: keyboard, language: language)
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

    if reachability.currentReachabilityStatus() == NotReachable {
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: installableKeyboards, error: error)
      return
    }

    let isUpdate = latestKeyboardFileVersion(withID: keyboard.id) != nil

    downloadQueue = HTTPDownloader.init(self)
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
      request.destinationFile = Storage.active.fontURL(forFilename: url.lastPathComponent).path
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
    guard let latestDownloadedVersion = latestKeyboardFileVersion(withID: keyboardID) else {
      return .needsDownload
    }

    // Check version
    if let latestRepositoryVersion = keyboardsInfo?[keyboardID]?.version,
      compareVersions(latestDownloadedVersion, latestRepositoryVersion) == .orderedAscending {
      return .needsUpdate
    }
    return .upToDate
  }

  /// - Precondition: `languages` is set.
  private func createKeyboardsInfo() {
    let keyboardsWithID = languages.flatMap { language in
      language.keyboards!.map { kb in (kb.id, kb) }
    }
    keyboardsInfo = Dictionary(keyboardsWithID, uniquingKeysWith: { (old, _) in old })
    let keyboardsWithLanguage = languages.flatMap { language -> [(String, InstallableKeyboard)] in
      language.keyboards!.map { kb in
        return ("\(language.id)_\(kb.id)", InstallableKeyboard(keyboard: kb, language: language))
      }
    }
    keyboardsDictionary = Dictionary(uniqueKeysWithValues: keyboardsWithLanguage)
    updateUserKeyboardsList()
  }

  private func updateUserKeyboardsList() {
    if keyboardsDictionary.isEmpty {
      return
    }
    let userData = Storage.active.userDefaults

    let lastVersion = userData.string(forKey: Key.engineVersion) ?? "1.0"
    if compareVersions(lastVersion, sdkVersion) == .orderedSame {
      return
    }
    userData.set(sdkVersion, forKey: Key.engineVersion)

    guard var userKbList = userData.userKeyboards else {
      kmLog("No user keyboards to update", checkDebugPrinting: true)
      return
    }

    for i in userKbList.indices {
      let kbID = userKbList[i].id
      let langID = userKbList[i].languageID
      if var kb = repositoryKeyboard(withID: kbID, languageID: langID) {
        kb.version = latestKeyboardFileVersion(withID: kbID)!
        kb.isCustom = false
        userKbList[i] = kb
      } else {
        var kb = userKbList[i]
        kb.isCustom = true
        userKbList[i] = kb
      }
    }
    userData.userKeyboards = userKbList
    userData.synchronize()
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

  @objc func reachabilityChanged(_ notification: Notification) {
    if isDebugPrintingOn {
      var reachStr = "Not Reachable"
      let status: NetworkStatus = reachability.currentReachabilityStatus()
      if status == ReachableViaWiFi {
        reachStr = "Reachable Via WiFi"
      }
      if status == ReachableViaWWAN {
        reachStr = "Reachable Via WWan"
      }
      kmLog("Reachability changed to '\(reachStr)'", checkDebugPrinting: true)
    }
  }

  // MARK: - HTTPDownloadDelegate methods

  func downloadQueueFinished(_ queue: HTTPDownloader) {
    if isDebugPrintingOn {
      let fontContents = try? FileManager.default.contentsOfDirectory(atPath: Storage.active.fontDir.path)
      kmLog("Font Directory contents: \(String(describing: fontContents))", checkDebugPrinting: true)
      let langContents = try? FileManager.default.contentsOfDirectory(atPath: Storage.active.languageDir.path)
      kmLog("Language Directory contents: \(String(describing: langContents))", checkDebugPrinting: true)
    }
  }

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
          registerCustomFonts()
          kmLog("Downloaded keyboard: \(keyboard.id).", checkDebugPrinting: true)

          NotificationCenter.default.post(name: Notifications.keyboardDownloadCompleted,
                                          object: self,
                                          value: keyboards)
          if isUpdate {
            shouldReloadKeyboard = true
            reloadKeyboard(in: inputView)
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
        kmLog("Keyboard download failed: \(error).", checkDebugPrinting: true)

        if !isUpdate {
          // Clean up keyboard file if anything fails
          // TODO: Also clean up remaining fonts
          try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboard))
        }
        downloadFailed(forKeyboards: keyboards, error: error)
      }
    case .downloadCachedData:
      if request == currentRequest {
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .secondsSince1970
        let result: LanguagesAPICall
        do {
          result = try decoder.decode(LanguagesAPICall.self, from: request.rawResponseData!)
        } catch {
          kmLog("Failed: \(error).", checkDebugPrinting: true)
          let error = NSError(domain: "Keyman", code: 0,
                              userInfo: [NSLocalizedDescriptionKey: error.localizedDescription])
          NotificationCenter.default.post(name: Notifications.languagesDownloadFailed, object: self, value: error)
          return
        }

        options = result.options
        languages = result.languages.sorted { a, b -> Bool in
          a.name.localizedCaseInsensitiveCompare(b.name) == .orderedAscending
        }

        createKeyboardsInfo()
        kmLog("Request completed -- \(languages.count) languages.", checkDebugPrinting: true)
        currentRequest = nil

        if let completionBlock = request.userInfo["completionBlock"] as? FetchKeyboardsBlock {
          completionBlock(nil)
        }

        NotificationCenter.default.post(name: Notifications.languagesUpdated, object: self, value: ())
      }
    }
  }

  func downloadRequestFailed(_ request: HTTPDownloadRequest) {
    switch request.typeCode {
    case .downloadFile:
      downloadQueue = nil
      let error = request.error!
      kmLog("Keyboard download failed: \(error).", checkDebugPrinting: true)

      let keyboards = request.userInfo[Key.keyboardInfo] as! [InstallableKeyboard]
      let keyboard = keyboards[0]
      let isUpdate = request.userInfo[Key.update] as! Bool

      if !isUpdate {
        // Clean up keyboard file if anything fails
        // TODO: Also clean up remaining fonts
        try? FileManager.default.removeItem(at: Storage.active.keyboardURL(for: keyboard))
      }
      downloadFailed(forKeyboards: keyboards, error: error as NSError)
    case .downloadCachedData:
      if request == currentRequest {
        let error = request.error!
        kmLog("Failed: \(error).", checkDebugPrinting: true)

        currentRequest = nil

        if let completionBlock = request.userInfo["completionBlock"] as? FetchKeyboardsBlock {
          completionBlock([NSUnderlyingErrorKey: error])
        }
        NotificationCenter.default.post(name: Notifications.languagesDownloadFailed, object: self, value: error)
      }
    }
  }

  private func downloadFailed(forKeyboards keyboards: [InstallableKeyboard], error: Error) {
    let notification = KeyboardDownloadFailedNotification(keyboards: keyboards, error: error)
    NotificationCenter.default.post(name: Notifications.keyboardDownloadFailed,
                                    object: self,
                                    value: notification)
  }

  // MARK: - Loading custom keyboards

  /// Preloads a .js file for a language so that the keyboard is available without downloading.
  /// - Precondition:
  ///   - The .js filename must remain the same as when obtained from Keyman.
  ///   - The .js file must be bundled in your application.
  public func preloadKeyboardFile(at url: URL, shouldOverwrite: Bool) throws {
    try Storage.copyAndExcludeFromBackup(at: url,
                                         to: Storage.active.languageDir.appendingPathComponent(url.lastPathComponent),
                                         shouldOverwrite: shouldOverwrite)
  }

  /// Preloads a .ttf or .otf file to be available without downloading.
  /// - Precondition:
  ///   - The font file must be bundled in your application.
  /// - SeeAlso: `registerCustomFonts()`
  public func preloadFontFile(at url: URL, shouldOverwrite: Bool) throws {
    try Storage.copyAndExcludeFromBackup(at: url,
                                         to: Storage.active.fontDir.appendingPathComponent(url.lastPathComponent),
                                         shouldOverwrite: shouldOverwrite)
  }

  /// Registers all new fonts found in the font path. Call this after you have preloaded all your font files
  /// with `preloadFontFile(atPath:shouldOverwrite:)`
  public func registerCustomFonts() {
    let directoryContents: [String]
    do {
      directoryContents = try FileManager.default.contentsOfDirectory(atPath: Storage.active.fontDir.path)
    } catch {
      kmLog("Failed to list font dir contents: \(error)", checkDebugPrinting: false)
      return
    }

    for fontFilename in directoryContents where fontFilename.hasFontExtension {
      if let fontInfo = keymanFonts[fontFilename] {
        if !fontInfo.isRegistered {
          if let newFontInfo = registerFont(withFilename: fontFilename) {
            keymanFonts[fontFilename] = newFontInfo
          }
        }
      } else if let fontInfo = registerFont(withFilename: fontFilename) {
        keymanFonts[fontFilename] = fontInfo
      }
    }
  }

  /// Unregisters all registered fonts in the font path.
  public func unregisterCustomFonts() {
    let directoryContents: [String]
    do {
      directoryContents = try FileManager.default.contentsOfDirectory(atPath: Storage.active.fontDir.path)
    } catch {
      kmLog("Failed to list font dir contents: \(error)", checkDebugPrinting: false)
      return
    }

    for fontFilename in directoryContents where fontFilename.hasFontExtension {
      if var fontInfo = keymanFonts[fontFilename], fontInfo.isRegistered {
        if unregisterFont(withFilename: fontFilename) {
          fontInfo.isRegistered = false
          keymanFonts[fontFilename] = fontInfo
        }
      }
    }
  }

  private func registerFont(withFilename fontFilename: String) -> RegisteredFont? {
    let fontURL = Storage.active.fontDir.appendingPathComponent(fontFilename)
    if !FileManager.default.fileExists(atPath: fontURL.path) {
      return nil
    }

    guard let provider = CGDataProvider(url: fontURL as CFURL) else {
      kmLog("Failed to open \(fontURL)", checkDebugPrinting: false)
      return nil
    }
    guard let font = CGFont(provider),
          let cfFontName = font.postScriptName else {
      kmLog("Failed to read font at \(fontURL)", checkDebugPrinting: false)
      return nil
    }

    var didRegister = false
    let fontName = cfFontName as String
    if !fontExists(fontName) {
      var errorRef: Unmanaged<CFError>?
      didRegister = CTFontManagerRegisterFontsForURL(fontURL as CFURL, .none, &errorRef)
      let error = errorRef?.takeRetainedValue() // Releases errorRef
      if !didRegister {
        kmLog("Failed to register font: \(fontURL) reason: \(error!.localizedDescription)",
          checkDebugPrinting: false)
      } else {
        kmLog("Registered font: \(fontURL)", checkDebugPrinting: true)
      }
    }
    return RegisteredFont(name: fontName, isRegistered: didRegister)
  }

  private func unregisterFont(withFilename fontFilename: String) -> Bool {
    let fontURL = Storage.active.fontDir.appendingPathComponent(fontFilename)
    if !FileManager.default.fileExists(atPath: fontURL.path) {
      return false
    }
    var errorRef: Unmanaged<CFError>?
    let didUnregister = CTFontManagerUnregisterFontsForURL(fontURL as CFURL, .none, &errorRef)
    let error = errorRef?.takeRetainedValue() // Releases errorRef
    if !didUnregister {
      kmLog("Failed to unregister font: \(fontURL) reason: \(error!.localizedDescription)", checkDebugPrinting: false)
    } else {
      kmLog("Unregistered font: \(fontFilename)", checkDebugPrinting: true)
    }
    return didUnregister
  }

  private func fontExists(_ fontName: String) -> Bool {
    return UIFont.familyNames.contains { familyName in
      UIFont.fontNames(forFamilyName: familyName).contains(fontName)
    }
  }

  // TODO: Use a logging library or have more than 2 log levels
  // Facilitates KeymanEngine internal logging.
  public func kmLog(_ logStr: String, checkDebugPrinting: Bool) {
    if checkDebugPrinting && !isDebugPrintingOn {
      return
    }
    NSLog("%@", logStr)
  }

  // MARK: - File system and UserData management
  private func copyUserDefaultsToSharedContainer() {
    guard let sharedUserDefaults = Storage.shared?.userDefaults,
      let defaultUserDefaults = Storage.nonShared?.userDefaults
    else {
      return
    }
    let keysToCopy = [Key.userKeyboardsList, Key.userCurrentKeyboard,
                      Key.engineVersion, Key.keyboardPickerDisplayed]
    for key in keysToCopy {
      if sharedUserDefaults.object(forKey: key) == nil {
        sharedUserDefaults.set(defaultUserDefaults.object(forKey: key), forKey: key)
      }
    }
    sharedUserDefaults.synchronize()
  }

  private func copyUserDefaultsFromSharedContainer() {
    guard let sharedUserDefaults = Storage.shared?.userDefaults,
      let defaultUserDefaults = Storage.nonShared?.userDefaults
    else {
        return
    }
    let keysToCopy = [Key.userKeyboardsList, Key.engineVersion]
    for key in keysToCopy {
      if sharedUserDefaults.object(forKey: key) != nil {
        defaultUserDefaults.set(sharedUserDefaults.object(forKey: key), forKey: key)
      }
    }
    defaultUserDefaults.synchronize()
  }

  func latestKeyboardFileVersion(withID keyboardID: String) -> String? {
    guard let dirContents = try? FileManager.default.contentsOfDirectory(atPath: Storage.active.languageDir.path) else {
      return nil
    }

    var latestVersion: String?
    for filename in dirContents where filename.hasPrefix("\(keyboardID)-") && filename.hasJavaScriptExtension {
      let dashRange = filename.range(of: "-", options: .backwards)!
      let extensionRange = filename.range(of: ".js", options: .backwards)!
      let version = String(filename[dashRange.upperBound..<extensionRange.lowerBound])

      if let previousMax = latestVersion {
        if compareVersions(previousMax, version) == .orderedAscending {
          latestVersion = version
        }
      } else if compareVersions(version, version) != nil {  // Ensure that the version number is valid
        latestVersion = version
      }
    }
    return latestVersion
  }

  /// Compares version numbers in dotted numberic format.
  /// - Returns: ComparisonResult if both version numbers are valid.
  func compareVersions(_ v1: String, _ v2: String) -> ComparisonResult? {
    if v1.isEmpty || v2.isEmpty {
      return nil
    }
    let components1 = v1.components(separatedBy: ".")
    let components2 = v2.components(separatedBy: ".")

    let len = max(components1.count, components2.count)
    for i in 0..<len {
      // Shorter version number padded with trailing zero components
      let component1 = components1[safe: i] ?? "0"
      let component2 = components2[safe: i] ?? "0"
      guard let val1 = Int(component1), val1 >= 0 else {
        return nil
      }
      guard let val2 = Int(component2), val2 >= 0 else {
        return nil
      }
      if val1 < val2 {
        return .orderedAscending
      }
      if val1 > val2 {
        return .orderedDescending
      }
    }
    return .orderedSame
  }

  func updateKeyboardVersion(forID kbID: String, newKeyboardVersion kbVersion: String) {
    let userData = Storage.active.userDefaults
    guard var userKeyboards = userData.userKeyboards else {
      return
    }

    // Set version in user keyboards list
    for i in userKeyboards.indices {
      var kb = userKeyboards[i]
      if kbID == kb.id {
        kb.version = kbVersion
        userKeyboards[i] = kb
      }
    }
    userData.userKeyboards = userKeyboards
    userData.synchronize()

    // Set version for current keyboard
    // TODO: Move this UserDefaults into a function
    let currentUserData = isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
    if var userKb = currentUserData.currentKeyboard {
      if kbID == userKb.id {
        userKb.version = kbVersion
        currentUserData.currentKeyboard = userKb
        currentUserData.synchronize()
      }
    }
  }

  func synchronizeSWKeyboard() {
    copyUserDefaultsFromSharedContainer()
    if let shared = Storage.shared,
      let nonShared = Storage.nonShared {
      do {
        try shared.copyFiles(to: nonShared)
        registerCustomFonts()
      } catch {
        kmLog("Failed to copy from shared container: \(error)", checkDebugPrinting: false)
      }
    }
  }

  // MARK: - View management

  public var keyboardHeight: CGFloat {
    if isSystemKeyboard {
      return keyboardHeight(isPortrait: InputViewController.isPortrait)
    } else {
      return keyboardHeight(isPortrait: UIDevice.current.orientation.isPortrait)
    }
  }

  func keyboardHeight(with orientation: UIInterfaceOrientation) -> CGFloat {
    return keyboardHeight(isPortrait: orientation.isPortrait)
  }

  func keyboardHeight(isPortrait: Bool) -> CGFloat {
    if UIDevice.current.userInterfaceIdiom == .pad {
      if isPortrait {
        return isSystemKeyboard ? padPortraitSystemKeyboardHeight : padPortraitInAppKeyboardHeight
      } else {
        return isSystemKeyboard ? padLandscapeSystemKeyboardHeight : padLandscapeInAppKeyboardHeight
      }
    } else {
      if isPortrait {
        return isSystemKeyboard ? phonePortraitSystemKeyboardHeight : phonePortraitInAppKeyboardHeight
      } else {
        return isSystemKeyboard ? phoneLandscapeSystemKeyboardHeight : phoneLandscapeInAppKeyboardHeight
      }
    }
  }

  var keyboardWidth: CGFloat {
    return UIScreen.main.bounds.width
  }

  var keyboardSize: CGSize {
    return CGSize(width: keyboardWidth, height: keyboardHeight)
  }

  // Keyman interaction
  private func resizeKeyboard() {
    let newSize = keyboardSize
    if didResizeToOrientation && isSystemKeyboard && lastKeyboardSize == newSize {
      didResizeToOrientation = false
      return
    }
    lastKeyboardSize = newSize

    inputView!.frame = CGRect(origin: .zero, size: newSize)

    // Workaround for WKWebView bug with landscape orientation
    // TODO: Check if still necessary and if there's a better solution
    if isSystemKeyboard {
      perform(#selector(self.resizeDelay), with: self, afterDelay: 1.0)
    }

    var oskHeight = Int(newSize.height)
    oskHeight -= oskHeight % (isSystemKeyboard ? 10 : 20)

    inputView.evaluateJavaScript("setOskWidth(\(Int(newSize.width)));", completionHandler: nil)
    inputView.evaluateJavaScript("setOskHeight(\(Int(oskHeight)));", completionHandler: nil)
  }

  private var keymanScrollView: UIScrollView {
    return inputView.scrollView
  }

  // TODO: Move to separate class
  private func createInputView() -> WKWebView {
    let config = WKWebViewConfiguration()
    let prefs = WKPreferences()
    prefs.javaScriptEnabled = true
    config.preferences = prefs
    config.suppressesIncrementalRendering = false
    let userContentController = WKUserContentController()
    userContentController.add(self, name: "keyman")
    config.userContentController = userContentController
    let frame = CGRect(origin: .zero, size: keyboardSize)
    let view = WKWebView(frame: frame, configuration: config)
    view.isOpaque = false
    view.backgroundColor = UIColor.clear
    view.navigationDelegate = self

    view.scrollView.isScrollEnabled = false
    reloadKeyboard(in: view)
    return view
  }

  @objc func clearSubKeyArrays() {
    if subKeysView == nil {
      subKeys.removeAll()
      subKeyIDs.removeAll()
      subKeyTexts.removeAll()
    }
  }

  @objc func dismissHelpBubble() {
    if let view = helpBubbleView {
      view.removeFromSuperview()
      helpBubbleView = nil
    }
  }

  @objc func dismissKeyPreview() {
    if let view = keyPreviewView {
      view.removeFromSuperview()
      keyPreviewView = nil
    }
  }

  var isSubKeysMenuVisible: Bool {
    return subKeysView != nil
  }

  private func dismissSubKeys() {
    if let subKeysView = subKeysView {
      subKeysView.removeFromSuperview()
      subKeysView.subviews.forEach { $0.removeFromSuperview() }
      self.subKeysView = nil
      setPopupVisible(false)
      NotificationCenter.default.post(name: Notifications.subKeysMenuDismissed, object: self, value: ())
    }
    subKeys.removeAll()
    subKeyIDs.removeAll()
    subKeyTexts.removeAll()
  }

  private func setPopupVisible(_ visible: Bool) {
    // FIXME: Looking at KMW, the parameter should be a bool
    let jsString = "popupVisible(\(visible ? "1" : "0"));"
    inputView.evaluateJavaScript(jsString, completionHandler: nil)
  }

  /// Displays a list of available keyboards and allows a user to add/download new keyboards
  /// or remove existing ones.
  ///
  /// - Parameters:
  ///   - in: The current UIViewController (recommended) or the navigation controller
  ///   - shouldAddKeyboard: Whether to immediately open the view to add a new keyboard
  /// - SeeAlso:
  /// TextView/TextField to enable/disable the keyboard picker
  public func showKeyboardPicker(in viewController: UIViewController, shouldAddKeyboard: Bool) {
    let vc = KeyboardPickerViewController()
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
    viewController.dismiss(animated: true)
    if shouldReloadKeyboard {
      reloadKeyboard(in: inputView)
    }
    NotificationCenter.default.post(name: Notifications.keyboardPickerDismissed, object: self, value: ())
  }

  private func reloadKeyboard(in view: WKWebView) {
    if #available(iOS 9.0, *) {
      view.loadFileURL(Storage.active.kmwURL, allowingReadAccessTo: Storage.active.baseDir)
    } else {
      // WKWebView in iOS < 9 is missing loadFileURL().
      view.load(URLRequest(url: Storage.active.kmwURL, cachePolicy: .reloadIgnoringCacheData, timeoutInterval: 60.0))
    }
  }

  @objc func resetKeyboard() {
    let keyboard = currentKeyboardInfo
    keyboardID = nil
    languageID = nil

    if let keyboard = keyboard {
      setKeyboard(keyboard)
    } else if let keyboard = Storage.active.userDefaults.userKeyboards?[safe: 0] {
      setKeyboard(keyboard)
    } else {
      setKeyboard(Constants.defaultKeyboard)
    }
  }

  @objc func showHelpBubble() {
    // Help bubble is always disabled for system-wide keyboard
    if isSystemKeyboard || keyboardMenuView != nil {
      return
    }

    let jsString = "langMenuPos();"
    inputView.evaluateJavaScript(jsString) { result, _ in
      guard let result = result else {
        return
      }
      let langMenuKeyPos = String(describing: result)
      let pos = langMenuKeyPos.components(separatedBy: ",")
      guard let px = Float(pos[0]), let py = Float(pos[1]) else {
        self.kmLog("Unexpected result for langMenuPos(): \(langMenuKeyPos)", checkDebugPrinting: false)
        return
      }
      self.showHelpBubble(at: CGPoint(x: CGFloat(px), y: CGFloat(py)))
    }
  }

  // TODO: The bulk of this should be moved to PopoverView
  func showHelpBubble(at point: CGPoint) {
    self.helpBubbleView?.removeFromSuperview()
    let helpBubbleView = PopoverView(frame: CGRect.zero)
    self.helpBubbleView = helpBubbleView
    helpBubbleView.backgroundColor = UIColor(red: 253.0 / 255.0, green: 244.0 / 255.0,
                                             blue: 196.0 / 255.0, alpha: 1.0)
    helpBubbleView.backgroundColor2 = UIColor(red: 233.0 / 255.0, green: 224.0 / 255.0,
                                              blue: 176.0 / 255.0, alpha: 1.0)
    helpBubbleView.borderColor = UIColor(red: 0.5, green: 0.25, blue: 0.25, alpha: 1.0)

    let isPad = UIDevice.current.userInterfaceIdiom == .pad
    let sizeMultiplier = CGFloat(isPad ? 1.5 : 1.0)
    let frameWidth = 90.0 * sizeMultiplier
    let frameHeight = (40.0 + helpBubbleView.arrowHeight) * sizeMultiplier
    let fontSize = 10.0 * sizeMultiplier

    let inputViewFrame = inputView.frame
    let screenWidth = inputViewFrame.size.width

    // TODO: Refactor this out
    let isPortrait: Bool
    if isSystemKeyboard {
      isPortrait = InputViewController.isPortrait
    } else {
      isPortrait = UIDevice.current.orientation.isPortrait
    }

    let adjY: CGFloat
    if isPortrait {
      adjY = isSystemKeyboard ? 9.0 : 4.0
    } else {
      adjY = isSystemKeyboard ? 3.0 : 4.0
    }
    let px = point.x
    let py = point.y + adjY + (isPad ? 2.0 : 1.0)
    var x = px - frameWidth / 2
    let y = py - frameHeight
    if x < 0 {
      x = 0
    } else if x + frameWidth > screenWidth {
      x = screenWidth - frameWidth
    }

    helpBubbleView.frame = CGRect(x: x, y: y, width: frameWidth, height: frameHeight)
    if x == 0 {
      helpBubbleView.arrowPosX = px
    } else if x == screenWidth - frameWidth {
      helpBubbleView.arrowPosX = (px - x)
    } else {
      helpBubbleView.arrowPosX = frameWidth / 2
    }

    let helpText = UILabel(frame: CGRect(x: 5, y: 0,
                                         width: frameWidth - 10, height: frameHeight - helpBubbleView.arrowHeight))
    helpText.backgroundColor = UIColor.clear
    helpText.font = helpText.font.withSize(fontSize)
    helpText.textAlignment = .center
    helpText.textColor = UIColor.darkText
    helpText.lineBreakMode = .byWordWrapping
    helpText.numberOfLines = 0
    helpText.text = keyboardChangeHelpText
    helpBubbleView.addSubview(helpText)
    inputView.addSubview(helpBubbleView)
  }

  @objc func resizeDelay() {
    // + 1000 to work around iOS bug with resizing on landscape orientation. Technically we only
    // need this for landscape but it doesn't hurt to do it with both. 1000 is a big number that
    // should hopefully work on all devices.
    let kbWidth = keyboardWidth
    let kbHeight = keyboardHeight
    inputView?.frame = CGRect(x: 0.0, y: 0.0, width: kbWidth, height: kbHeight + 1000)
  }

  func resizeKeyboardIfNeeded() {
    // TODO: Check if necessary since resizeKeyboard() checks old size
    let newSize = keyboardSize
    if newSize != lastKeyboardSize {
      resizeKeyboard()
      lastKeyboardSize = newSize
    }
  }

  func resizeKeyboard(with orientation: UIInterfaceOrientation) {
    // TODO: Update to use new size instead of orientation since viewWillRotate() is deprecated
    // TODO: Refactor to use resizeKeyboard()
    let kbWidth = keyboardWidth
    let kbHeight = keyboardHeight(with: orientation)
    inputView?.frame = CGRect(x: 0.0, y: 0.0, width: kbWidth, height: kbHeight)

    var oskHeight = Int(kbHeight)
    oskHeight -= oskHeight % (isSystemKeyboard ? 10 : 20)

    inputView?.evaluateJavaScript("setOskWidth(\(Int(kbWidth)));", completionHandler: nil)
    inputView?.evaluateJavaScript("setOskHeight(\(Int(oskHeight)));", completionHandler: nil)
  }

  // MARK: - WKScriptMessageHandler methods
  public func userContentController(_ userContentController: WKUserContentController,
                                    didReceive message: WKScriptMessage) {
    guard let fragment = message.body as? String else {
      return
    }

    if fragment.hasPrefix("ios-log:") {
      let requestString = fragment.removingPercentEncoding ?? fragment
      let logString = requestString.components(separatedBy: ":#iOS#")[1]
      kmLog("WebView: \(logString)", checkDebugPrinting: false)
    } else {
      webDelegate?.updatedFragment(fragment)
      inputDelegate?.updatedFragment(fragment)
      updatedFragment(fragment)
    }
  }

  // MARK: - Text

  // TODO: Switch from NSRange
  func setSelectionRange(_ range: NSRange, manually: Bool) {
    if range.location != NSNotFound {
      let jsString = "setCursorRange(\(range.location),\(range.length))"
      inputView.evaluateJavaScript(jsString, completionHandler: nil)
    }
  }

  func clearText() {
    setText(nil)
    setSelectionRange(NSRange(location: 0, length: 0), manually: true)
    kmLog("Cleared text.", checkDebugPrinting: true)
  }

  func setText(_ text: String?) {
    var text = text ?? ""
    text = text.replacingOccurrences(of: "\\", with: "\\\\")
    text = text.replacingOccurrences(of: "'", with: "\\'")
    text = text.replacingOccurrences(of: "\n", with: "\\n")
    let jsString = "setKeymanVal('\(text)');"
    inputView.evaluateJavaScript(jsString, completionHandler: nil)
  }

  // MARK: - Keyboard Notifications
  @objc func keyboardWillShow(_ notification: Notification) {
    dismissSubKeys()
    dismissKeyPreview()
    resizeKeyboard()

    if isKeymanHelpOn {
      helpBubbleView?.removeFromSuperview()
      let showHelpBubble = #selector(self.showHelpBubble as () -> Void)
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: showHelpBubble, object: nil)
      perform(showHelpBubble, with: nil, afterDelay: 1.5)
    }
  }

  @objc func keyboardWillHide(_ notification: Notification) {
    dismissHelpBubble()
    dismissSubKeys()
    dismissKeyPreview()
  }

  // MARK: - WKNavigationDelegate methods
  public func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
    guard let url = webView.url else {
      return
    }
    guard url.lastPathComponent == Constants.kmwFileName && (url.fragment?.isEmpty ?? true) else {
      return
    }

    kmLog("Loaded keyboard.", checkDebugPrinting: true)
    resizeKeyboard()
    let deviceType = (UIDevice.current.userInterfaceIdiom == .phone) ? "AppleMobile" : "AppleTablet"
    webView.evaluateJavaScript("setDeviceType('\(deviceType)');", completionHandler: nil)

    var newKb = Constants.defaultKeyboard
    if (keyboardID == nil || languageID == nil) && !shouldReloadKeyboard {
      let userData = isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
      if let currentKb = userData.currentKeyboard {
        let kbID = currentKb.id
        let langID = currentKb.languageID
        if Storage.active.userDefaults.userKeyboard(withID: kbID, languageID: langID) != nil {
          newKb = currentKb
        }
      } else if let userKbs = Storage.active.userDefaults.userKeyboards, !userKbs.isEmpty {
        newKb = userKbs[0]
      }
      setKeyboard(newKb)
    }

    NotificationCenter.default.post(name: Notifications.keyboardLoaded, object: self, value: newKb)
    if shouldReloadKeyboard {
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: #selector(self.resetKeyboard), object: nil)
      perform(#selector(self.resetKeyboard), with: nil, afterDelay: 0.25)
      shouldReloadKeyboard = false
    }
  }

  // MARK: - Keyman Web Events
  func updatedFragment(_ fragment: String) {
    if fragment.isEmpty {
      return
    }

    // TODO: Parse the fragment into an enum of possible commands with parameters parsed.
    // updatedFragment() will take this enum instead of a String.
    if fragment.contains("showKeyPreview") {
      processShowKeyPreview(fragment)
    } else if fragment.contains("dismissKeyPreview") {
      let isPad = UIDevice.current.userInterfaceIdiom == .pad
      if isPad || keyPreviewView == nil {
        return
      }
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: #selector(self.dismissKeyPreview), object: nil)
      perform(#selector(self.dismissKeyPreview), with: nil, afterDelay: 0.1)
      clearSubKeyArrays()
    } else if fragment.contains("insertText") {
      dismissHelpBubble()
      isKeymanHelpOn = false
    } else if fragment.contains("menuKeyUp") {
      dismissHelpBubble()
      isKeymanHelpOn = false
      if isSystemKeyboard {
        let userData = UserDefaults.standard
        userData.set(true, forKey: Key.keyboardPickerDisplayed)
        userData.synchronize()
      }
    } else if fragment.contains("hideKeyboard") {
      dismissHelpBubble()
      dismissSubKeys()
      dismissKeyboardMenu()
    } else if fragment.contains("showMore") {
      processShowMore(fragment)
    }
  }

  private func getKeyFrameWith(x: CGFloat, y: CGFloat, w: CGFloat, h: CGFloat) -> CGRect {
    let isPad: Bool = UIDevice.current.userInterfaceIdiom == .pad
    let adjY: CGFloat = isPad ? -0.5 : -1.0
    let frame = CGRect(x: x - w / 2.0, y: y - adjY, width: w, height: h)
    return frame
  }

  private func processShowKeyPreview(_ fragment: String) {
    if UIDevice.current.userInterfaceIdiom == .pad || (isSystemKeyboard && !isSystemKeyboardTopBarEnabled) ||
      subKeysView != nil {
      return
    }

    dismissKeyPreview()
    clearSubKeyArrays()

    // Fragment in the form "showKeyPreview-\(fragmentToggle)+x=\(x)+y=\(y)+w=\(w)+h=\(h)+t=\(t)"
    let xKey = fragment.range(of: "+x=")!
    let yKey = fragment.range(of: "+y=")!
    let wKey = fragment.range(of: "+w=")!
    let hKey = fragment.range(of: "+h=")!
    let tKey = fragment.range(of: "+t=")!
    let x = CGFloat(Float(fragment[xKey.upperBound..<yKey.lowerBound])!)
    let y = CGFloat(Float(fragment[yKey.upperBound..<wKey.lowerBound])!)
    let w = CGFloat(Float(fragment[wKey.upperBound..<hKey.lowerBound])!)
    let h = CGFloat(Float(fragment[hKey.upperBound..<tKey.lowerBound])!)
    let t = String(fragment[tKey.upperBound...])

    keyFrame = getKeyFrameWith(x: x, y: y, w: w, h: h)
    keyPreviewView = KeyPreviewView(frame: keyFrame)

    let text = t.stringFromUTF16CodeUnits()

    keyPreviewView!.setLabelText(text!)
    var oskFontName = oskFontNameForKeyboard(withID: keyboardID!, languageID: languageID!)
    oskFontName = oskFontName ?? fontNameForKeyboard(withID: keyboardID!, languageID: languageID!)
    keyPreviewView!.setLabelFont(oskFontName)
    inputView.addSubview(keyPreviewView!)
  }

  private func processShowMore(_ fragment: String) {
    dismissHelpBubble()
    isKeymanHelpOn = false
    dismissSubKeys()
    dismissKeyboardMenu()

    // Fragment in the form "showMore-\(fragmentToggle)+baseFrame=\(baseFrame)+keys=\(keys)+font=\(font)"
    // Font parameter is optional
    let baseFrameKey = fragment.range(of: "+baseFrame=")!
    let keysKey = fragment.range(of: "+keys=")!
    let fontKey = fragment.range(of: "+font=")
    let baseFrame = fragment[baseFrameKey.upperBound..<keysKey.lowerBound]
    let keys = fragment[keysKey.upperBound..<(fontKey?.lowerBound ?? fragment.endIndex)]
    specialOSKFont = fontKey.map { String(fragment[$0.upperBound...]) }

    let frameComponents = baseFrame.components(separatedBy: ",")
    let x = CGFloat(Float(frameComponents[0])!)
    let y = CGFloat(Float(frameComponents[1])!)
    let w = CGFloat(Float(frameComponents[2])!)
    let h = CGFloat(Float(frameComponents[3])!)
    keyFrame = getKeyFrameWith(x: x, y: y, w: w, h: h)

    let keyArray = keys.components(separatedBy: ";")
    subKeyIDs = keyArray
    subKeyTexts = keyArray
    for i in keyArray.indices {
      let values = keyArray[i].components(separatedBy: ":")
      if values.count == 2 {
        subKeyIDs[i] = values[0]
        subKeyTexts[i] = values[1]
      } else if values.count == 1 {
        subKeyIDs[i] = values[0]
        var subKeyText = values[0]
        if let index = subKeyText.index(of: "-") {
          subKeyText = String(subKeyText[subKeyText.index(after: index)...])
        }
        if let index = subKeyText.index(of: "_") {
          subKeyText = String(subKeyText[subKeyText.index(after: index)...])
        }
        let unicode = "0x\(subKeyText)"
        subKeyTexts[i] = unicode
      } else {
        subKeyIDs[i] = ""
        subKeyTexts[i] = ""
      }
    }
  }

  // MARK: - UIGestureRecognizer
  public func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer,
                                shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer) -> Bool {
    return true
  }

  // UILongPressGestureRecognizer implementation to show sub keys in a subview
  @objc func holdAction(_ sender: UILongPressGestureRecognizer) {
    switch sender.state {
    case .ended:
      // Touch Ended
      if let subKeysView = subKeysView {
        subKeysView.removeFromSuperview()
        subKeysView.subviews.forEach { $0.removeFromSuperview() }
        self.subKeysView = nil
        setPopupVisible(false)
        NotificationCenter.default.post(name: Notifications.subKeysMenuDismissed, object: self, value: ())
      }
      var buttonClicked = false
      for button in subKeys where button.isHighlighted {
        button.isHighlighted = false
        button.backgroundColor = subKeyColor
        button.isEnabled = false
        button.sendActions(for: .touchUpInside)
        buttonClicked = true
        break
      }
      if !buttonClicked {
        clearSubKeyArrays()
      }
    case .began:
      // Touch & Hold Began
      let touchPoint = sender.location(in: sender.view)
      // Check if touch was for language menu button
      inputView.evaluateJavaScript("langMenuPos();") { result, _ in
        let keyFrame = result as! String
        self.setMenuKeyFrame(keyFrame)
        if self.menuKeyFrame.contains(touchPoint) {
          self.inputDelegate?.updatedFragment("showKeyboardMenu")
          return
        }
        self.touchHoldBegan()
      }
    default:
      // Hold & Move
      guard let subKeysView = subKeysView else {
        kmLog("Unexpected hold and move while subKeysView = nil", checkDebugPrinting: false)
        return
      }
      let touchPoint = sender.location(in: subKeysView.containerView)
      for button in subKeys {
        if button.frame.contains(touchPoint) {
          button.isEnabled = true
          button.isHighlighted = true
          button.backgroundColor = subKeyColorHighlighted
        } else {
          button.isHighlighted = false
          button.isEnabled = false
          button.backgroundColor = subKeyColor
        }
      }
    }
  }

  private func touchHoldBegan() {
    let isPad = UIDevice.current.userInterfaceIdiom == .pad
    let fontSize = isPad ? UIFont.buttonFontSize * 2 : UIFont.buttonFontSize

    var oskFontName = oskFontNameForKeyboard(withID: keyboardID!, languageID: languageID!)
    if oskFontName == nil {
      oskFontName = fontNameForKeyboard(withID: keyboardID!, languageID: languageID!)
    }

    if subKeyIDs.isEmpty {
      subKeys = []
      return
    }

    subKeys = subKeyTexts.enumerated().map { i, subKeyText in
      let button = UIButton(type: .custom)
      button.tag = i
      button.backgroundColor = subKeyColor
      button.setRoundedBorder(withRadius: 4.0, borderWidth: 1.0, color: .gray)
      button.setTitleColor(.black, for: .disabled)
      button.setTitleColor(.black, for: .highlighted)

      if let oskFontName = oskFontName {
        button.titleLabel?.font = UIFont(name: oskFontName, size: fontSize)
      } else {
        button.titleLabel?.font = UIFont.systemFont(ofSize: fontSize)
      }

      if specialOSKFont != nil {
        button.titleLabel?.font = UIFont(name: "KeymanwebOsk", size: fontSize)
        button.setTitleColor(.gray, for: .disabled)
      }

      let buttonTitle = subKeyText.contains("0x") ? subKeyText.stringFromUTF16CodeUnits()! : subKeyText

      button.addTarget(self, action: #selector(subKeyButtonClick), for: .touchUpInside)
      button.setTitle(buttonTitle, for: .normal)
      button.tintColor = UIColor(red: 181.0 / 255.0, green: 181.0 / 255.0, blue: 181.0 / 255.0, alpha: 1.0)
      button.isEnabled = false
      return button
    }

    dismissKeyPreview()
    subKeysView = SubKeysView(keyFrame: keyFrame, subKeys: subKeys)
    NotificationCenter.default.post(name: Notifications.subKeysMenuWillShow, object: self, value: ())
    inputView.addSubview(subKeysView!)
    setPopupVisible(true)
  }

  @objc func subKeyButtonClick(_ sender: UIButton) {
    let keyIndex = sender.tag
    if keyIndex < subKeyIDs.count && keyIndex < subKeyTexts.count {
      let subKeyID = subKeyIDs[keyIndex]
      let subKeyText = subKeyTexts[keyIndex]
      let jsString = "executePopupKey('\(subKeyID)','\(subKeyText)');"
      inputView.evaluateJavaScript(jsString, completionHandler: nil)
    }
    subKeys.removeAll()
    subKeyIDs.removeAll()
    subKeyTexts.removeAll()
  }

  // MARK: - InputViewController methods
  // TODO: Manager should not have InputViewController methods. Move this into InputViewController.
  func updateViewConstraints() {
    dismissSubKeys()
    dismissKeyPreview()
    dismissKeyboardMenu()
    resizeKeyboardIfNeeded()
  }

  func inputViewDidLoad() {
    dismissSubKeys()
    dismissKeyPreview()
    dismissKeyboardMenu()
    resizeKeyboard()

    let activeUserDef = Storage.active.userDefaults
    let standardUserDef = UserDefaults.standard
    let activeDate = (activeUserDef.object(forKey: Key.synchronizeSWKeyboard) as? [Date])?[safe: 0]
    let standardDate = (standardUserDef.object(forKey: Key.synchronizeSWKeyboard) as? [Date])?[safe: 0]

    let shouldSynchronize: Bool
    if let standardDate = standardDate,
       let activeDate = activeDate {
      shouldSynchronize = standardDate != activeDate
    } else if activeDate == nil {
      shouldSynchronize = false
    } else {
      shouldSynchronize = true
    }

    if (!didSynchronize || shouldSynchronize) && Storage.shared != nil {
      synchronizeSWKeyboard()
      if keyboardID != nil && languageID != nil {
        shouldReloadKeyboard = true
        reloadKeyboard(in: inputView)
      }
      didSynchronize = true
      standardUserDef.set(activeUserDef.object(forKey: Key.synchronizeSWKeyboard),
                          forKey: Key.synchronizeSWKeyboard)
      standardUserDef.synchronize()
    }
  }

  // FIXME: This is deprecated. Use inputViewWillTransition()
  func inputViewWillRotate(to toInterfaceOrientation: UIInterfaceOrientation, duration: TimeInterval) {
    dismissSubKeys()
    dismissKeyPreview()
    dismissKeyboardMenu()
    resizeKeyboard(with: toInterfaceOrientation)
    if isKeymanHelpOn {
      helpBubbleView?.removeFromSuperview()
      let showHelpBubble = #selector(self.showHelpBubble as () -> Void)
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: showHelpBubble, object: nil)
      perform(showHelpBubble, with: nil, afterDelay: 1.5)
    }
    didResizeToOrientation = true
  }

  private func setMenuKeyFrame(_ frameStr: String) {
    var frame = CGRect.zero
    if !frameStr.isEmpty {
      let values = frameStr.components(separatedBy: ",")
      let x = CGFloat(Float(values[0])!)
      let y = CGFloat(Float(values[1])!)
      let w = CGFloat(Float(values[2])!)
      let h = CGFloat(Float(values[3])!)
      let isPad = UIDevice.current.userInterfaceIdiom == .pad
      let adjY: CGFloat = isPad ? -0.5 : -1.0
      frame = CGRect(x: x - w / 2.0, y: y - adjY, width: w, height: h)
    }
    menuKeyFrame = frame
  }

  func showKeyboardMenu(_ ic: InputViewController, closeButtonTitle: String?) {
    let parentView = ic.view ?? inputView
    inputView.evaluateJavaScript("langMenuPos();") { result, _ in
      let keyFrame = result as! String
      self.setMenuKeyFrame(keyFrame)
      if self.menuKeyFrame != .zero {
        self.keyboardMenuView?.removeFromSuperview()
        self.keyboardMenuView = KeyboardMenuView(keyFrame: self.menuKeyFrame, inputViewController: ic,
                                                 closeButtonTitle: closeButtonTitle)
        parentView?.addSubview(self.keyboardMenuView!)
      }
    }
  }

  func dismissKeyboardMenu() {
    if let keyboardMenuView = keyboardMenuView {
      keyboardMenuView.removeFromSuperview()
      self.keyboardMenuView = nil
    }
  }

  var isKeyboardMenuVisible: Bool {
    return keyboardMenuView != nil
  }

  var isSystemKeyboardTopBarEnabled: Bool {
    return true
  }
}
