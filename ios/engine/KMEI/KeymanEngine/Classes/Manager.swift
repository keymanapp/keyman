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

  /// In keyboard extensions (system keyboard), `UIApplication.openURL(_:)` is unavailable. The API is not called in
  /// the system keyboard since `KeyboardInfoViewController` is never used. `openURL(:_)` is only used in applications,
  /// where it is safe. However, the entire Keyman Engine framework must be compiled with extension-safe APIs.
  ///
  /// Set this to `UIApplication.shared.openURL` in your application.
  public var openURL: ((URL) -> Bool)?

  var currentKeyboardID: FullKeyboardID?
  var currentRequest: HTTPDownloadRequest?
  var shouldReloadKeyboard = false

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

    reachability = Reachability(hostName: keymanHostName)

    if(!Util.isSystemKeyboard) {
      NotificationCenter.default.addObserver(self, selector: #selector(self.reachabilityChanged),
                                           name: .reachabilityChanged, object: reachability)
      reachability.startNotifier()
    }

    /* HTTPDownloader only uses this for its delegate methods.  So long as we don't
     * set the queue running, this should be perfectly fine.
     */
    sharedQueue = HTTPDownloader.init(self)

    // We used to preload the old KeymanWebViewController, but now that it's embedded within the
    // InputViewController, that's not exactly viable.
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
  //TODO: this method appears unused, should we remove it?
  public func setKeyboard(withFullID fullID: FullKeyboardID) -> Bool {
    if let keyboard = Storage.active.userDefaults.userKeyboard(withFullID: fullID) {
        return setKeyboard(keyboard)
    }
    return false
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
    if let index = userKeyboards.index(where: { $0.fullID == keyboard.fullID }) {
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
  public func parseKMP(_ folder: URL) throws -> Void {
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
                  isRTL: false,
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
              Manager.shared.addKeyboard(keyboard)
            }
          }
        }
      }
    } catch {
      log.error("error parsing kmp: \(error)")
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

  // MARK: - Text

  public func showKeyboard() {
    currentResponder?.summonKeyboard()
  }
  
  public func hideKeyboard() {
    currentResponder?.dismissKeyboard()
    Manager.shared.inputViewController.resetKeyboardState()
  }

  func setText(_ text: String?) {
    inputViewController.setText(text)
  }

  func clearText() {
    inputViewController.clearText()
  }

  func setSelectionRange(_ range: NSRange, manually: Bool) {
    inputViewController.setSelectionRange(range, manually: manually)
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
