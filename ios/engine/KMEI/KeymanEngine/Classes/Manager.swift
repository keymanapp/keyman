//
//  Manager.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

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

// URLs
private let keymanHostName = "r.keymanweb.com"

public class Manager: NSObject, HTTPDownloadDelegate, UIGestureRecognizerDelegate, KeymanWebDelegate {
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

  /// In keyboard extensions (system keyboard), `UIApplication.openURL(_:)` is unavailable. The API is not called in
  /// the system keyboard since `KeyboardInfoViewController` is never used. `openURL(:_)` is only used in applications,
  /// where it is safe. However, the entire Keyman Engine framework must be compiled with extension-safe APIs.
  ///
  /// Set this to `UIApplication.shared.openURL` in your application.
  public var openURL: ((URL) -> Bool)?

  var keyboardID: String?
  var languageID: String?
  weak var keymanWebDelegate: KeymanWebDelegate?
  var currentRequest: HTTPDownloadRequest?
  var shouldReloadKeyboard = false
  var keymanWeb: KeymanWebViewController!

  private var downloadQueue: HTTPDownloader?
  private var sharedQueue: HTTPDownloader!
  private var reachability: Reachability!
  private var didSynchronize = false

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
    super.init()

    URLProtocol.registerClass(KeymanURLProtocol.self)

    if !Util.isSystemKeyboard {
      if let shared = Storage.shared,
        let nonShared = Storage.nonShared {
        let keysToCopy = [Key.userKeyboardsList, Key.userCurrentKeyboard,
                          Key.engineVersion, Key.keyboardPickerDisplayed]
        nonShared.copyUserDefaults(to: shared, withKeys: keysToCopy, shouldOverwrite: false)
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
      try Storage.active.copyKMWFiles(from: Resources.bundle)
    } catch {
      kmLog("Failed to copy KMW files from bundle: \(error)", checkDebugPrinting: false)
    }

    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillShow),
                                           name: .UIKeyboardWillShow, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillHide),
                                           name: .UIKeyboardWillHide, object: nil)

    updateUserKeyboards(with: Defaults.keyboard)

    reachability = Reachability(hostName: keymanHostName)
    NotificationCenter.default.addObserver(self, selector: #selector(self.reachabilityChanged),
                                           name: .reachabilityChanged, object: reachability)
    reachability.startNotifier()

    /* HTTPDownloader only uses this for its delegate methods.  So long as we don't
     * set the queue running, this should be perfectly fine.
     */
    sharedQueue = HTTPDownloader.init(self)

    keymanWeb = KeymanWebViewController()
    keymanWeb.delegate = self
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
      if (self.keyboardID == nil || self.languageID == nil) && kb.id != Defaults.keyboard.id {
        _ = setKeyboard(Defaults.keyboard)
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

    if let fontFilename = kb.font?.source.first(where: { $0.hasFontExtension }) {
      _ = FontManager.shared.registerFont(at: Storage.active.fontURL(forFilename: fontFilename))
    }
    if let oskFontFilename = kb.oskFont?.source.first(where: { $0.hasFontExtension }) {
      _ = FontManager.shared.registerFont(at: Storage.active.fontURL(forFilename: oskFontFilename))
    }

    keymanWeb.setKeyboard(id: kb.id, name: kb.name, languageID: kb.languageID, languageName: kb.languageName,
                          version: kbVersion, font: jsFont, oskFont: jsOskFont)

    let userData = Util.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults

    userData.currentKeyboard = kb
    userData.synchronize()

    if isKeymanHelpOn {
      keymanWeb.showHelpBubble(afterDelay: 1.5)
    }

    NotificationCenter.default.post(name: Notifications.keyboardChanged,
                                    object: self,
                                    value: kb)
    return true
  }

  /// Adds a new keyboard to the list in the keyboard picker if it doesn't already exist.
  /// The keyboard must be downloaded (see `downloadKeyboard()`) or preloaded (see `preloadLanguageFile()`)
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
  ///   - The keyboard info is not available in the user keyboards list
  public func fontNameForKeyboard(withID keyboardID: String, languageID: String) -> String? {
    let kb = Storage.active.userDefaults.userKeyboard(withID: keyboardID, languageID: languageID)
    if let filename = kb?.font?.source.first(where: { $0.hasFontExtension }) {
      let fontURL = Storage.active.fontURL(forFilename: filename)
      return FontManager.shared.fontName(at: fontURL)
    }
    return nil
  }

  /// - Returns: the OSK font name for the given keyboard ID and languageID, or returns nil if
  ///   - The keyboard doesn't have an OSK font
  ///   - The keyboard info is not available in the user keyboards list
  func oskFontNameForKeyboard(withID keyboardID: String, languageID: String) -> String? {
    let kb = Storage.active.userDefaults.userKeyboard(withID: keyboardID, languageID: languageID)
    if let filename = kb?.oskFont?.source.first(where: { $0.hasFontExtension }) {
      let fontURL = Storage.active.fontURL(forFilename: filename)
      return FontManager.shared.fontName(at: fontURL)
    }
    return nil
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
        kmLog("Fetching repository from API for keyboard download", checkDebugPrinting: true)
        apiKeyboardRepository.fetch { error in
          if let error = error {
            self.downloadFailed(forKeyboards: [], error: error)
          } else {
            self.kmLog("Fetched repository. Continuing with keyboard download.", checkDebugPrinting: true)
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

    guard reachability.currentReachabilityStatus() != NotReachable else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboards: [keyboard], error: error)
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
    if let latestRepositoryVersion = apiKeyboardRepository.keyboards?[keyboardID]?.version,
      compareVersions(latestDownloadedVersion, latestRepositoryVersion) == .orderedAscending {
      return .needsUpdate
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
          FontManager.shared.registerCustomFonts()
          kmLog("Downloaded keyboard: \(keyboard.id).", checkDebugPrinting: true)

          NotificationCenter.default.post(name: Notifications.keyboardDownloadCompleted,
                                          object: self,
                                          value: keyboards)
          if isUpdate {
            shouldReloadKeyboard = true
            keymanWeb.reloadKeyboard()
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

  // TODO: Use a logging library or have more than 2 log levels
  // Facilitates KeymanEngine internal logging.
  public func kmLog(_ logStr: String, checkDebugPrinting: Bool) {
    if checkDebugPrinting && !isDebugPrintingOn {
      return
    }
    NSLog("%@", logStr)
  }

  // MARK: - File system and UserData management
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
          kb.version = newKeyboard.id
        }
        userKeyboards[i] = kb
      }
    }
    userData.userKeyboards = userKeyboards
    userData.synchronize()

    // Set version for current keyboard
    let currentUserData = Util.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
    if var kb = currentUserData.currentKeyboard {
      if kb.id == newKeyboard.id {
        if kb.languageID == newKeyboard.languageID {
          kb = newKeyboard
        } else {
          kb.version = newKeyboard.id
        }
        currentUserData.currentKeyboard = kb
        currentUserData.synchronize()
      }
    }
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
        kmLog("Failed to copy from shared container: \(error)", checkDebugPrinting: false)
      }
    }
  }

  // MARK: - View management
  public func loadKeyboard() {
    _ = keymanWeb.view
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
      keymanWeb.reloadKeyboard()
    }
    NotificationCenter.default.post(name: Notifications.keyboardPickerDismissed, object: self, value: ())
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
      setKeyboard(Defaults.keyboard)
    }
  }

  // MARK: - Text

  // TODO: Switch from NSRange
  func setSelectionRange(_ range: NSRange, manually: Bool) {
    if range.location != NSNotFound {
      keymanWeb.setCursorRange(range)
    }
  }

  func clearText() {
    setText(nil)
    setSelectionRange(NSRange(location: 0, length: 0), manually: true)
    kmLog("Cleared text.", checkDebugPrinting: true)
  }

  func setText(_ text: String?) {
    keymanWeb.setText(text)
  }

  // MARK: - Keyboard Notifications
  @objc func keyboardWillShow(_ notification: Notification) {
    keymanWeb.dismissSubKeys()
    keymanWeb.dismissKeyPreview()
    keymanWeb.resizeKeyboard()

    if isKeymanHelpOn {
      keymanWeb.showHelpBubble(afterDelay: 1.5)
    }
  }

  @objc func keyboardWillHide(_ notification: Notification) {
    keymanWeb.dismissHelpBubble()
    keymanWeb.dismissSubKeys()
    keymanWeb.dismissKeyPreview()
  }

  // MARK: - KeymanWebViewDelegate methods
  func keyboardLoaded(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.keyboardLoaded(keymanWeb)

    kmLog("Loaded keyboard.", checkDebugPrinting: true)
    keymanWeb.resizeKeyboard()
    keymanWeb.setDeviceType(UIDevice.current.userInterfaceIdiom)

    var newKb = Defaults.keyboard
    if (keyboardID == nil || languageID == nil) && !shouldReloadKeyboard {
      let userData = Util.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
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

  func insertText(_ keymanWeb: KeymanWebViewController, numCharsToDelete: Int, newText: String) {
    keymanWebDelegate?.insertText(keymanWeb, numCharsToDelete: numCharsToDelete, newText: newText)
  }

  func showKeyPreview(_ keymanWeb: KeymanWebViewController, keyFrame: CGRect, preview: String) {
    keymanWebDelegate?.showKeyPreview(keymanWeb, keyFrame: keyFrame, preview: preview)
  }

  func dismissKeyPreview(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.dismissKeyPreview(keymanWeb)
  }

  func showSubkeys(_ keymanWeb: KeymanWebViewController,
                   keyFrame: CGRect,
                   subkeyIDs: [String],
                   subkeyTexts: [String],
                   useSpecialFont: Bool) {
    keymanWebDelegate?.showSubkeys(keymanWeb,
                                   keyFrame: keyFrame,
                                   subkeyIDs: subkeyIDs,
                                   subkeyTexts: subkeyTexts,
                                   useSpecialFont: useSpecialFont)
  }

  func menuKeyDown(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.menuKeyDown(keymanWeb)
  }

  func menuKeyUp(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.menuKeyUp(keymanWeb)
  }

  func hideKeyboard(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.hideKeyboard(keymanWeb)
  }

  // MARK: - InputViewController methods
  // TODO: Manager should not have InputViewController methods. Move this into InputViewController.
  func updateViewConstraints() {
    keymanWeb.dismissSubKeys()
    keymanWeb.dismissKeyPreview()
    keymanWeb.dismissKeyboardMenu()
    keymanWeb.resizeKeyboard()
  }

  func inputViewDidLoad() {
    keymanWeb.dismissSubKeys()
    keymanWeb.dismissKeyPreview()
    keymanWeb.dismissKeyboardMenu()
    keymanWeb.resizeKeyboard()

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
        keymanWeb.reloadKeyboard()
      }
      didSynchronize = true
      standardUserDef.set(activeUserDef.object(forKey: Key.synchronizeSWKeyboard),
                          forKey: Key.synchronizeSWKeyboard)
      standardUserDef.synchronize()
    }
  }

  // FIXME: This is deprecated. Use inputViewWillTransition()
  func inputViewWillRotate(to toInterfaceOrientation: UIInterfaceOrientation, duration: TimeInterval) {
    keymanWeb.dismissSubKeys()
    keymanWeb.dismissKeyPreview()
    keymanWeb.dismissKeyboardMenu()
    keymanWeb.resizeKeyboard(with: toInterfaceOrientation)
    if isKeymanHelpOn {
      keymanWeb.showHelpBubble(afterDelay: 1.5)
    }
  }

  var isSystemKeyboardTopBarEnabled: Bool {
    return true
  }
}
