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

public class Manager: NSObject, HTTPDownloadDelegate, UIGestureRecognizerDelegate, KeymanWebDelegate {
  /// Application group identifier for shared container. Set this before accessing the shared manager.
  public static var applicationGroupIdentifier: String?

  public static let shared = Manager()

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
  var keymanWeb: KeymanWebViewController! = nil

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

    Migrations.migrateForKMP(storage: Storage.active)

    if Util.isSystemKeyboard || Storage.active.userDefaults.bool(forKey: Key.keyboardPickerDisplayed) {
      isKeymanHelpOn = false
    }

    do {
      try Storage.active.copyKMWFiles(from: Resources.bundle)
    } catch {
      log.error("Failed to copy KMW files from bundle: \(error)")
    }

    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillShow),
                                           name: .UIKeyboardWillShow, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillHide),
                                           name: .UIKeyboardWillHide, object: nil)

    updateUserKeyboards(with: Defaults.keyboard)

    keymanWeb = KeymanWebViewController(storage: Storage.active)
    keymanWeb.frame = CGRect(origin: .zero, size: keyboardSize)
    keymanWeb.delegate = self
    reloadKeyboard(in: keymanWeb)

    // Set UILongPressGestureRecognizer to show sub keys
    // TODO: Move to KeymanWebViewController
    let hold = UILongPressGestureRecognizer(target: self, action: #selector(self.holdAction))
    hold.minimumPressDuration = 0.5
    hold.delegate = self
    keymanWeb.view.addGestureRecognizer(hold)

    reachability = Reachability(hostName: keymanHostName)
    NotificationCenter.default.addObserver(self, selector: #selector(self.reachabilityChanged),
                                           name: .reachabilityChanged, object: reachability)
    reachability.startNotifier()

    /* HTTPDownloader only uses this for its delegate methods.  So long as we don't
     * set the queue running, this should be perfectly fine.
     */
    sharedQueue = HTTPDownloader.init(self)
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
      log.info("Keyboard unchanged: \(kb.languageID)_\(kb.id)")
      return false
    }

    log.info("Setting language: \(kb.languageID)_\(kb.id)")

    self.languageID = kb.languageID
    self.keyboardID = kb.id

    if let fontFilename = kb.font?.source.first(where: { $0.hasFontExtension }) {
      _ = FontManager.shared.registerFont(at: Storage.active.fontURL(forKeyboardID: kb.id, filename: fontFilename))
    }
    if let oskFontFilename = kb.oskFont?.source.first(where: { $0.hasFontExtension }) {
      _ = FontManager.shared.registerFont(at: Storage.active.fontURL(forKeyboardID: kb.id, filename: oskFontFilename))
    }

    keymanWeb.setKeyboard(kb)

    let userData = Util.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults

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

    log.info("Removing keyboard with ID \(kb.id) and languageID \(kb.languageID)")

    // Set a new keyboard if deleting the current one
    if kb.id == keyboardID && kb.languageID == languageID {
      setKeyboard(userKeyboards[0])
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
      let fontURL = Storage.active.fontURL(forKeyboardID: keyboardID, filename: filename)
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
      let fontURL = Storage.active.fontURL(forKeyboardID: keyboardID, filename: filename)
      return FontManager.shared.fontName(at: fontURL)
    }
    return nil
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

    guard reachability.currentReachabilityStatus() != NotReachable else {
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

  @objc func reachabilityChanged(_ notification: Notification) {
    log.debug {
      let reachStr: String
      switch reachability.currentReachabilityStatus() {
      case ReachableViaWiFi:
        reachStr = "Reachable Via WiFi"
      case ReachableViaWWAN:
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
            reloadKeyboard(in: keymanWeb)
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

  // MARK: - Loading custom keyboards
  /// Preloads the JS and font files required for a keyboard.
  public func preloadFiles(forKeyboardID keyboardID: String, at urls: [URL], shouldOverwrite: Bool) throws {
    let keyboardDir = Storage.active.keyboardDir(forID: keyboardID)
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
        log.error("Failed to copy from shared container: \(error)")
      }
    }
  }

  // MARK: - View management

  public var keyboardHeight: CGFloat {
    if Util.isSystemKeyboard {
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
        return Util.isSystemKeyboard ? padPortraitSystemKeyboardHeight : padPortraitInAppKeyboardHeight
      } else {
        return Util.isSystemKeyboard ? padLandscapeSystemKeyboardHeight : padLandscapeInAppKeyboardHeight
      }
    } else {
      if isPortrait {
        return Util.isSystemKeyboard ? phonePortraitSystemKeyboardHeight : phonePortraitInAppKeyboardHeight
      } else {
        return Util.isSystemKeyboard ? phoneLandscapeSystemKeyboardHeight : phoneLandscapeInAppKeyboardHeight
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
    if didResizeToOrientation && Util.isSystemKeyboard && lastKeyboardSize == newSize {
      didResizeToOrientation = false
      return
    }
    lastKeyboardSize = newSize

    keymanWeb.frame = CGRect(origin: .zero, size: newSize)

    // Workaround for WKWebView bug with landscape orientation
    // TODO: Check if still necessary and if there's a better solution
    if Util.isSystemKeyboard {
      perform(#selector(self.resizeDelay), with: self, afterDelay: 1.0)
    }

    var oskHeight = Int(newSize.height)
    oskHeight -= oskHeight % (Util.isSystemKeyboard ? 10 : 20)

    keymanWeb.setOskWidth(Int(newSize.width))
    keymanWeb.setOskHeight(oskHeight)
  }

  private var keymanScrollView: UIScrollView {
    return keymanWeb.webView.scrollView
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
      keymanWeb.setPopupVisible(false)
    }
    subKeys.removeAll()
    subKeyIDs.removeAll()
    subKeyTexts.removeAll()
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
      reloadKeyboard(in: keymanWeb)
    }
    NotificationCenter.default.post(name: Notifications.keyboardPickerDismissed, object: self, value: ())
  }

  private func reloadKeyboard(in keymanWeb: KeymanWebViewController) {
    if #available(iOS 9.0, *) {
      keymanWeb.webView.loadFileURL(Storage.active.kmwURL, allowingReadAccessTo: Storage.active.baseDir)
    } else {
      // WKWebView in iOS < 9 is missing loadFileURL().
      let request = URLRequest(url: Storage.active.kmwURL,
                               cachePolicy: .reloadIgnoringCacheData,
                               timeoutInterval: 60.0)
      keymanWeb.webView.load(request)
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
      setKeyboard(Defaults.keyboard)
    }
  }

  @objc func showHelpBubble() {
    // Help bubble is always disabled for system-wide keyboard
    if Util.isSystemKeyboard || keyboardMenuView != nil {
      return
    }

    keymanWeb.languageMenuPosition { keyFrame in
      self.showHelpBubble(at: keyFrame.origin)
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

    let inputViewFrame = keymanWeb.view.frame
    let screenWidth = inputViewFrame.size.width

    // TODO: Refactor this out
    let isPortrait: Bool
    if Util.isSystemKeyboard {
      isPortrait = InputViewController.isPortrait
    } else {
      isPortrait = UIDevice.current.orientation.isPortrait
    }

    let adjY: CGFloat
    if isPortrait {
      adjY = Util.isSystemKeyboard ? 9.0 : 4.0
    } else {
      adjY = Util.isSystemKeyboard ? 3.0 : 4.0
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
    keymanWeb.view.addSubview(helpBubbleView)
  }

  @objc func resizeDelay() {
    // + 1000 to work around iOS bug with resizing on landscape orientation. Technically we only
    // need this for landscape but it doesn't hurt to do it with both. 1000 is a big number that
    // should hopefully work on all devices.
    let kbWidth = keyboardWidth
    let kbHeight = keyboardHeight
    keymanWeb.frame = CGRect(x: 0.0, y: 0.0, width: kbWidth, height: kbHeight + 1000)
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
    keymanWeb.frame = CGRect(x: 0.0, y: 0.0, width: kbWidth, height: kbHeight)

    var oskHeight = Int(kbHeight)
    oskHeight -= oskHeight % (Util.isSystemKeyboard ? 10 : 20)

    keymanWeb.setOskWidth(Int(kbWidth))
    keymanWeb.setOskHeight(oskHeight)
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
    log.info("Cleared text.")
  }

  func setText(_ text: String?) {
    keymanWeb.setText(text)
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

  // MARK: - KeymanWebViewDelegate methods
  func keyboardLoaded(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.keyboardLoaded(keymanWeb)

    log.info("Loaded keyboard.")
    resizeKeyboard()
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

    dismissHelpBubble()
    isKeymanHelpOn = false
  }

  func showKeyPreview(_ keymanWeb: KeymanWebViewController, keyFrame: CGRect, preview: String) {
    keymanWebDelegate?.showKeyPreview(keymanWeb, keyFrame: keyFrame, preview: preview)

    if UIDevice.current.userInterfaceIdiom == .pad
      || (Util.isSystemKeyboard && !isSystemKeyboardTopBarEnabled)
      || subKeysView != nil {
      return
    }

    dismissKeyPreview()
    clearSubKeyArrays()

    keyPreviewView = KeyPreviewView(frame: keyFrame)

    keyPreviewView!.setLabelText(preview)
    var oskFontName = oskFontNameForKeyboard(withID: keyboardID!, languageID: languageID!)
    oskFontName = oskFontName ?? fontNameForKeyboard(withID: keyboardID!, languageID: languageID!)
    keyPreviewView!.setLabelFont(oskFontName)
    keymanWeb.view.addSubview(keyPreviewView!)
  }

  func dismissKeyPreview(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.dismissKeyPreview(keymanWeb)

    if UIDevice.current.userInterfaceIdiom == .pad || keyPreviewView == nil {
      return
    }

    let dismissKeyPreview = #selector(self.dismissKeyPreview as () -> Void)
    NSObject.cancelPreviousPerformRequests(withTarget: self, selector: dismissKeyPreview, object: nil)
    perform(dismissKeyPreview, with: nil, afterDelay: 0.1)
    clearSubKeyArrays()
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

    dismissHelpBubble()
    isKeymanHelpOn = false
    dismissSubKeys()
    dismissKeyboardMenu()

    self.keyFrame = keyFrame
    subKeyIDs = subkeyIDs
    subKeyTexts = subkeyTexts
  }

  func menuKeyDown(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.menuKeyDown(keymanWeb)
  }

  func menuKeyUp(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.menuKeyUp(keymanWeb)

    dismissHelpBubble()
    isKeymanHelpOn = false
    if Util.isSystemKeyboard {
      let userData = UserDefaults.standard
      userData.set(true, forKey: Key.keyboardPickerDisplayed)
      userData.synchronize()
    }
  }

  func hideKeyboard(_ keymanWeb: KeymanWebViewController) {
    keymanWebDelegate?.hideKeyboard(keymanWeb)

    dismissHelpBubble()
    dismissSubKeys()
    dismissKeyboardMenu()
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
        keymanWeb.setPopupVisible(false)
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
      keymanWeb.languageMenuPosition { keyFrame in
        self.menuKeyFrame = keyFrame
        if keyFrame.contains(touchPoint) {
          self.keymanWebDelegate?.menuKeyHeld(self.keymanWeb)
          return
        }
        self.touchHoldBegan()
      }
    default:
      // Hold & Move
      guard let subKeysView = subKeysView else {
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

      button.addTarget(self, action: #selector(subKeyButtonClick), for: .touchUpInside)
      button.setTitle(subKeyText, for: .normal)
      button.tintColor = UIColor(red: 181.0 / 255.0, green: 181.0 / 255.0, blue: 181.0 / 255.0, alpha: 1.0)
      button.isEnabled = false
      return button
    }

    dismissKeyPreview()
    subKeysView = SubKeysView(keyFrame: keyFrame, subKeys: subKeys)
    keymanWeb.view.addSubview(subKeysView!)
    keymanWeb.setPopupVisible(true)
  }

  @objc func subKeyButtonClick(_ sender: UIButton) {
    let keyIndex = sender.tag
    if keyIndex < subKeyIDs.count && keyIndex < subKeyTexts.count {
      let subKeyID = subKeyIDs[keyIndex]
      let subKeyText = subKeyTexts[keyIndex]
      keymanWeb.executePopupKey(id: subKeyID, text: subKeyText)
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
        reloadKeyboard(in: keymanWeb)
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

  func showKeyboardMenu(_ ic: InputViewController, closeButtonTitle: String?) {
    let parentView = ic.view ?? keymanWeb.view
    keymanWeb.languageMenuPosition { keyFrame in
      self.menuKeyFrame = keyFrame
      if keyFrame != .zero {
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
