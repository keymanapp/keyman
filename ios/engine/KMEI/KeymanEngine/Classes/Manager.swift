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
public enum KeyboardState: Int {
  case needsDownload
  case needsUpdate
  case upToDate
  case downloading
  case none
}

// TODO: Use a struct
// TODO: Remove kKeyman prefix and put in a constants class
// Common dictionary keys when handling language/keyboard info
public let kKeymanIdKey = "id"
public let kKeymanNameKey = "name"
// If there is no value for this key, revert to kKeymanIdKey
public let kKeymanKeyboardIdKey = "kbId"
// If there is no value for this key, revert to kKeymanIdKey
public let kKeymanLanguageIdKey = "langId"
// If there is no value for this key, revert to kKeymanNameKey
public let kKeymanKeyboardNameKey = "kbName"
// If there is no value for this key, revert to kKeymanNameKey
public let kKeymanLanguageNameKey = "langName"
// If there is no value for this key, revert to kKeymanNameKey
public let kKeymanKeyboardVersionKey = "version"
public let kKeymanKeyboardKey = "keyboard"
public let kKeymanLanguageKeyboardsKey = "keyboards"
public let kKeymanKeyboardFileSizeKey = "fileSize"
// If there is no value for this key, revert to kKeymanKeyboardFileSizeKey
public let kKeymanKeyboardGZipFileSizeKey = "fileSizeGzip"
public let kKeymanFontKey = "font"
public let kKeymanOskFontKey = "oskFont"
public let kKeymanFontFamilyKey = "family"
public let kKeymanFontSourceKey = "source"
public let kKeymanFontFilesKey = "files"
// Font filename is deprecated
public let kKeymanFontFilenameKey = "filename"
public let kKeymanFontNameKey = "fontname"
public let kKeymanFontRegisteredKey = "fontregistered"
public let kKeymanKeyboardFilenameKey = "filename"
public let kKeymanKeyboardModifiedKey = "lastModified"
public let kKeymanKeyboardRTLKey = "rtl"
public let kKeymanKeyboardInfoKey = "keyboardInfo"
public let kKeymanCustomKeyboardKey = "CustomKeyboard"

// Common user defaults keys

// Stores array of user keyboards info list
public let kKeymanUserKeyboardsListKey = "UserKeyboardsList"
// Stores currently/last selected keyboard info
public let kKeymanUserCurrentKeyboardKey = "UserCurrentKeyboard"

// Internal user defaults keys
private let kKeymanEngineVersionKey = "KeymanEngineVersion"
private let kKeymanKeyboardPickerDisplayedKey = "KeyboardPickerDisplayed"
private let kKeymanSynchronizeSWKeyboardKey = "KeymanSynchronizeSWKeyboard"

// Strings
private let kKeymanKeyboardChangeHelpText = "Tap here to change keyboard"

// URLs and Filenames
private let kKeymanApiBaseURL = "https://r.keymanweb.com/api/3.0/"
private let kKeymanApiRemoteURL = "https://r.keymanweb.com/api/2.0/remote?url="
private let kKeymanFileName = "keyboard"
private let kKeymanFileExtension = "html"
private let kKeymanFullFileName = "\(kKeymanFileName).\(kKeymanFileExtension)"
private let kKeymaniOSCodeFileName = "keymanios.js"
private let kKeymanHostName = "r.keymanweb.com"

// Default Keyboard Info
public let kKeymanDefaultKeyboardID = "european2"
public let kKeymanDefaultLanguageID = "eng"
public let kKeymanDefaultKeyboardName = "EuroLatin2 Keyboard"
public let kKeymanDefaultLanguageName = "English"
public let kKeymanDefaultKeyboardRTL = "N"
public let kKeymanDefaultKeyboardFont = "{\"family\":\"LatinWeb\",\"files\":[\"DejaVuSans.ttf\",\"DejaVuSans.mobileconfig\"]}"
public let kKeymanDefaultKeyboard = [
  kKeymanKeyboardIdKey: kKeymanDefaultKeyboardID,
  kKeymanLanguageIdKey: kKeymanDefaultLanguageID,
  kKeymanKeyboardNameKey: kKeymanDefaultKeyboardName,
  kKeymanLanguageNameKey: kKeymanDefaultLanguageName,
  kKeymanKeyboardRTLKey: kKeymanDefaultKeyboardRTL,
  kKeymanFontKey: kKeymanDefaultKeyboardFont
]

// JSON keys for language REST calls
let kKeymanOptionsKey = "options"
let kKeymanLanguageKey = "language"
// TODO: Check if it matches with the key in Keyman Cloud API
let kKeymanKeyboardCopyrightKey = "copyright"
private let kKeymanLanguagesKey = "languages"
private let kKeymanKeyboardBaseURIKey = "keyboardBaseUri"
private let kKeymanFontBaseURIKey = "fontBaseUri"
private let kKeymanKeyboardURIKey = "uri"

// Other keys
private let kKeymanUpdateKey = "update"

// UI In-App Keyboard Constants
private let kKeymanPhonePortraitInAppKeyboardHeight: CGFloat = 183.0
private let kKeymanPhoneLandscapeInAppKeyboardHeight: CGFloat = 183.0
private let kKeymanPadPortraitInAppKeyboardHeight: CGFloat = 385.0
private let kKeymanPadLandscapeInAppKeyboardHeight: CGFloat = 385.0

// UI System Keyboard Constants
private let kKeymanPhonePortraitSystemKeyboardHeight: CGFloat = 216.0
private let kKeymanPhoneLandscapeSystemKeyboardHeight: CGFloat = 162.0
private let kKeymanPadPortraitSystemKeyboardHeight: CGFloat = 264.0
private let kKeymanPadLandscapeSystemKeyboardHeight: CGFloat = 352.0

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
  /// - This list won't be available until the kKeymanLanguagesUpdatedNotification has been broadcasted
  public private(set) var languages: [[String: Any]] = []

  /// Dictionary of Keyman keyboards to store language and keyboard names etc.
  ///
  /// The key format is $languageID_$keyboardID. For example, "eng_european2" returns the English EuroLatin2 keyboard
  public private(set) var keyboardsDictionary: [String: [String: String]] = [:]

  /// Dictionary of available Keyman keyboard fonts
  public private(set) var keymanFonts: [String: [String: Any]] = [:]

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

  var keyboardID: String?
  var languageID: String?
  weak var webDelegate: KeymanWebViewDelegate?
  weak var inputDelegate: KeymanWebViewDelegate?
  var currentRequest: HTTPDownloadRequest?
  var keyboardsInfo: [String: [String: String]]?
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
  var options: [AnyHashable: Any] = [:]

  // MARK: - Object Admin
  deinit {
    NotificationCenter.default.removeObserver(self)
    if currentRequest?.userInfo != nil {
      currentRequest!.userInfo["completionBlock"] = nil
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
      copyKeymanFilesToSharedContainer()
      let userData = activeUserDefaults()
      let isKPDisplayed = userData.bool(forKey: kKeymanKeyboardPickerDisplayedKey)
      if isKPDisplayed {
        isKeymanHelpOn = false
      }
    } else {
      isKeymanHelpOn = false
    }

    copyWebFilesToLibrary()
    renameOldKeyboardFiles()

    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillShow),
                                           name: .UIKeyboardWillShow, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillHide),
                                           name: .UIKeyboardWillHide, object: nil)

    let kbVersion = latestKeyboardFileVersion(withID: kKeymanDefaultKeyboardID)
    updateKeyboardVersion(forID: kKeymanDefaultKeyboardID, newKeyboardVersion: kbVersion!)

    inputView = createInputView() // Pre-load keyboard

    // Set UILongPressGestureRecognizer to show sub keys
    let hold = UILongPressGestureRecognizer(target: self, action: #selector(self.holdAction))
    hold.minimumPressDuration = 0.5
    hold.delegate = self
    inputView.addGestureRecognizer(hold)

    reachability = Reachability(hostName: kKeymanHostName)
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

  // TODO: Lots of duplicated logic in the setKeyboard() functions

  /// Set the current keyboard.
  ///
  /// - Precondition:
  ///   - The language/keyboard list must be loaded (see `fetchKeyboardsList()`)
  ///   - The keyboard must be downloaded or preloaded.
  ///
  /// - SeeAlso:
  /// fetchKeyboardsList()
  /// downloadKeyboard(withID:languageID:isUpdate:)
  /// - Returns: Whether the keyboard was set successfully
  public func setKeyboard(withID keyboardID: String, languageID: String) -> Bool {
    if languageID == self.languageID && keyboardID == self.keyboardID {
      return false
    }

    kmLog("Setting language: \(languageID)_\(keyboardID)", checkDebugPrinting: true)
    if usingTempFolder {
      copyKeymanFilesToTemp()
    }

    if keyboardID.isEmpty || languageID.isEmpty {
      return false
    }

    let kbState = stateForKeyboard(withID: keyboardID)
    if kbState == .needsDownload {
      kmLog("Could not set keyboardID to \(keyboardID) because the keyboard file does not exist",
        checkDebugPrinting: false)
      if (self.keyboardID == nil || self.languageID == nil) && keyboardID != kKeymanDefaultKeyboardID {
        setKeyboard(withID: kKeymanDefaultKeyboardID, languageID: kKeymanDefaultLanguageID,
                    keyboardName: kKeymanDefaultKeyboardName, languageName: kKeymanDefaultLanguageName,
                    font: kKeymanDefaultKeyboardFont, oskFont: nil)
      }
      return false
    }

    self.languageID = languageID
    self.keyboardID = keyboardID
    let key = "\(languageID)_\(keyboardID)"

    let userKeyboards = activeUserDefaults().array(forKey: kKeymanUserKeyboardsListKey)
    let index = indexForUserKeyboard(withID: keyboardID, languageID: languageID)

    let kbDict: [String: String]?
    if let index = index {
      kbDict = userKeyboards![index] as? [String: String]
    } else {
      kbDict = keyboardsDictionary[key]
    }

    var langName = kbDict?[kKeymanLanguageNameKey]
    var kbName = kbDict?[kKeymanKeyboardNameKey]
    let kbVersion = latestKeyboardFileVersion(withID: keyboardID) ?? "1.0"
    let isRTL = kbDict?[kKeymanKeyboardRTLKey]

    if key == "\(kKeymanDefaultLanguageID)_\(kKeymanDefaultKeyboardID)" {
      if langName == nil || kbName == nil {
        langName = kKeymanDefaultLanguageName
        kbName = kKeymanDefaultKeyboardName
      }
    }

    var jsFont = self.jsFont(forKeyboardID: keyboardID, languageID: languageID)
    var jsOskFont = self.jsOskFont(forKeyboardID: keyboardID, languageID: languageID)
    if jsOskFont == "''" {
      jsOskFont = jsFont
    }
    let escapedLangName = langName!.replacingOccurrences(of: "'", with: "\\'")
    let escapedKbName = kbName!.replacingOccurrences(of: "'", with: "\\'")
    if jsFont == "''" {
      jsFont = "undefined"
    }
    if jsOskFont == "''" {
      jsOskFont = "undefined"
    }
    let jsString = "setKeymanLanguage('\(escapedKbName)','\(keyboardID)','\(escapedLangName)'," +
      "'\(languageID)','\(kbVersion)',\(jsFont),\(jsOskFont))"
    kmLog("Evaluating JavaScript: \(jsString)", checkDebugPrinting: true)
    inputView.evaluateJavaScript(jsString, completionHandler: nil)

    let userData = isSystemKeyboard ? UserDefaults.standard : activeUserDefaults()

    userData.set([
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanLanguageIdKey: languageID,
      kKeymanKeyboardNameKey: kbName,
      kKeymanLanguageNameKey: langName,
      kKeymanKeyboardVersionKey: kbVersion,
      kKeymanKeyboardRTLKey: isRTL ?? "N",
      kKeymanFontKey: jsFont
      ], forKey: kKeymanUserCurrentKeyboardKey)
    userData.synchronize()

    if isKeymanHelpOn {
      helpBubbleView?.removeFromSuperview()
      let showHelpBubble = #selector(self.showHelpBubble as () -> Void)
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: showHelpBubble, object: nil)
      perform(showHelpBubble, with: nil, afterDelay: 1.5)
    }

    let kbInfo = [
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanLanguageIdKey: languageID,
      kKeymanKeyboardNameKey: escapedKbName,
      kKeymanLanguageNameKey: escapedLangName,
      kKeymanKeyboardVersionKey: kbVersion,
      kKeymanFontKey: jsFont
    ]

    NotificationCenter.default.post(name: .keymanKeyboardChanged, object: self,
                                    userInfo: [kKeymanKeyboardInfoKey: kbInfo])
    return true
  }

  /// Set the current keyboard.
  ///
  /// - Precondition:
  ///   - The language/keyboard list must be loaded (see `fetchKeyboardsList()`)
  ///   - The keyboard must be downloaded or preloaded.
  ///
  /// - Parameters:
  ///   - keyboardName: Keyboard name for display on spacebar
  ///   - languageName: Language name for display on spacebar
  ///
  /// - SeeAlso:
  /// fetchKeyboardsList()
  /// downloadKeyboard(withID:languageID:isUpdate:)
  /// - Returns: Whether the keyboard was set successfully
  func setKeyboard(withID keyboardID: String, languageID: String,
                   keyboardName: String?, languageName: String?) -> Bool {
    if keyboardName == nil || languageName == nil {
      return setKeyboard(withID: keyboardID, languageID: languageID)
    }

    if languageID == self.languageID && keyboardID == self.keyboardID {
      return false
    }

    kmLog("Setting language: \(languageID)_\(keyboardID)", checkDebugPrinting: true)
    if usingTempFolder {
      copyKeymanFilesToTemp()
    }

    if keyboardID.isEmpty || languageID.isEmpty {
      return false
    }

    let kbState = stateForKeyboard(withID: keyboardID)
    if kbState == .needsDownload {
      kmLog("Could not set keyboardID to \(keyboardID) because the keyboard file does not exist",
        checkDebugPrinting: false)
      if (self.keyboardID == nil || self.languageID == nil) && keyboardID != kKeymanDefaultKeyboardID {
        setKeyboard(withID: kKeymanDefaultKeyboardID, languageID: kKeymanDefaultLanguageID,
                    keyboardName: kKeymanDefaultKeyboardName, languageName: kKeymanDefaultLanguageName,
                    font: kKeymanDefaultKeyboardFont, oskFont: nil)
      }
      return false
    }

    self.languageID = languageID
    self.keyboardID = keyboardID
    let key = "\(languageID)_\(keyboardID)"

    let userKeyboards = activeUserDefaults().array(forKey: kKeymanUserKeyboardsListKey)
    let index = indexForUserKeyboard(withID: keyboardID, languageID: languageID)

    let kbDict: [String: String]?
    if let index = index {
      kbDict = userKeyboards![index] as? [String: String]
    } else {
      kbDict = keyboardsDictionary[key]
    }

    let kbVersion = latestKeyboardFileVersion(withID: keyboardID) ?? "1.0"
    let isRTL = kbDict?[kKeymanKeyboardRTLKey]

    var jsFont = self.jsFont(forKeyboardID: keyboardID, languageID: languageID)
    var jsOskFont = self.jsOskFont(forKeyboardID: keyboardID, languageID: languageID)
    if jsOskFont == "''" {
      jsOskFont = jsFont
    }
    let escapedLangName = languageName!.replacingOccurrences(of: "'", with: "\\'")
    let escapedKbName = keyboardName!.replacingOccurrences(of: "'", with: "\\'")
    if jsFont == "''" {
      jsFont = "undefined"
    }
    if jsOskFont == "''" {
      jsOskFont = "undefined"
    }
    let jsString = "setKeymanLanguage('\(escapedKbName)','\(keyboardID)','\(escapedLangName)'," +
    "'\(languageID)','\(kbVersion)',\(jsFont),\(jsOskFont))"
    kmLog("Evaluating JavaScript: \(jsString)", checkDebugPrinting: true)
    inputView.evaluateJavaScript(jsString, completionHandler: nil)

    let userData = isSystemKeyboard ? UserDefaults.standard : activeUserDefaults()

    userData.set([
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanLanguageIdKey: languageID,
      kKeymanKeyboardNameKey: keyboardName,
      kKeymanLanguageNameKey: languageName,
      kKeymanKeyboardVersionKey: kbVersion,
      kKeymanKeyboardRTLKey: isRTL ?? "N",
      kKeymanFontKey: jsFont
      ], forKey: kKeymanUserCurrentKeyboardKey)
    userData.synchronize()

    if isKeymanHelpOn {
      helpBubbleView?.removeFromSuperview()
      let showHelpBubble = #selector(self.showHelpBubble as () -> Void)
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: showHelpBubble, object: nil)
      perform(showHelpBubble, with: nil, afterDelay: 1.5)
    }

    let kbInfo = [
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanLanguageIdKey: languageID,
      kKeymanKeyboardNameKey: escapedKbName,
      kKeymanLanguageNameKey: escapedLangName,
      kKeymanKeyboardVersionKey: kbVersion,
      kKeymanFontKey: jsFont
    ]

    NotificationCenter.default.post(name: .keymanKeyboardChanged, object: self,
                                    userInfo: [kKeymanKeyboardInfoKey: kbInfo])
    return true

  }

  /// Set the current keyboard.
  ///
  /// - Precondition:
  ///   - The language/keyboard list must be loaded (see fetchKeyboardsList())
  ///   - The keyboard must be downloaded or preloaded.
  ///
  /// - Parameters:
  ///   - keyboardName: Keyboard name for display on spacebar
  ///   - languageName: Language name for display on spacebar
  ///   - font: JSON font object or file name of preloaded font in the text field
  ///   - oskFont: JSON font object or file name of preloaded font for the on-screen keyboard
  ///
  /// - SeeAlso:
  /// fetchKeyboardsList()
  /// downloadKeyboard(withID:languageID:isUpdate:)
  /// preloadLanguageFile(atPath:shouldOverwrite:)
  /// keyboardDictionary
  /// preloadFontFile(atPath:shouldOverwrite:)
  /// - Returns: Whether the keyboard was set successfully
  public func setKeyboard(withID keyboardID: String, languageID: String,
                          keyboardName: String?, languageName: String?,
                          font: String?, oskFont: String?) -> Bool {
    // if the font and the oskFont are both nil call setKeyboardWithID without these parameters
    if font == nil && oskFont == nil {
      return setKeyboard(withID: keyboardID, languageID: languageID,
                               keyboardName: keyboardName, languageName: languageName)
    }

    if languageID == self.languageID && keyboardID == self.keyboardID {
      return false
    }

    kmLog("Setting language: \(languageID)_\(keyboardID)", checkDebugPrinting: true)
    if usingTempFolder {
      if !copyKeymanFilesToTemp() {
        return false
      }
    }

    if keyboardID.isEmpty || languageID.isEmpty {
      return false
    }

    let kbState = stateForKeyboard(withID: keyboardID)
    if kbState == .needsDownload {
      kmLog("Could not set keyboardID to \(keyboardID) because the keyboard file does not exist",
        checkDebugPrinting: false)
      if (self.keyboardID == nil || self.languageID == nil) && keyboardID != kKeymanDefaultKeyboardID {
        _ = setKeyboard(withID: kKeymanDefaultKeyboardID, languageID: kKeymanDefaultLanguageID,
                        keyboardName: kKeymanDefaultKeyboardName, languageName: kKeymanDefaultLanguageName,
                        font: kKeymanDefaultKeyboardFont, oskFont: nil)
      }
      return false
    }

    self.languageID = languageID
    self.keyboardID = keyboardID
    let key = "\(languageID)_\(keyboardID)"

    let userKeyboards = activeUserDefaults().array(forKey: kKeymanUserKeyboardsListKey)
    let index = indexForUserKeyboard(withID: keyboardID, languageID: languageID)

    let kbDict: [String: String]?
    if let index = index {
      kbDict = userKeyboards![index] as? [String: String]
    } else {
      kbDict = keyboardsDictionary[key]
    }

    let kbVersion = latestKeyboardFileVersion(withID: keyboardID) ?? "1.0"
    let isRTL = kbDict?[kKeymanKeyboardRTLKey]

    var jsFont = jsFontString(font)
    var jsOskFont: String
    if oskFont == nil || oskFont == "''" {
      jsOskFont = jsFont
    } else {
      jsOskFont = jsFontString(oskFont)
    }

    let escapedLangName = languageName!.replacingOccurrences(of: "'", with: "\\'")
    let escapedKbName = keyboardName!.replacingOccurrences(of: "'", with: "\\'")
    if jsFont == "''" {
      jsFont = "undefined"
    }
    if jsOskFont == "''" {
      jsOskFont = "undefined"
    }
    let jsString = "setKeymanLanguage('\(escapedKbName)','\(keyboardID)','\(escapedLangName)'," +
    "'\(languageID)','\(kbVersion)',\(jsFont),\(jsOskFont))"
    kmLog("Evaluating JavaScript: \(jsString)", checkDebugPrinting: true)
    inputView.evaluateJavaScript(jsString, completionHandler: nil)

    let userData = isSystemKeyboard ? UserDefaults.standard : activeUserDefaults()

    userData.set([
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanLanguageIdKey: languageID,
      kKeymanKeyboardNameKey: keyboardName,
      kKeymanLanguageNameKey: languageName,
      kKeymanKeyboardVersionKey: kbVersion,
      kKeymanKeyboardRTLKey: isRTL ?? "N",
      kKeymanFontKey: jsFont
      ], forKey: kKeymanUserCurrentKeyboardKey)
    userData.synchronize()

    if isKeymanHelpOn {
      helpBubbleView?.removeFromSuperview()
      let showHelpBubble = #selector(self.showHelpBubble as () -> Void)
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: showHelpBubble, object: nil)
      perform(showHelpBubble, with: nil, afterDelay: 1.5)
    }

    let kbInfo = [
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanLanguageIdKey: languageID,
      kKeymanKeyboardNameKey: escapedKbName,
      kKeymanLanguageNameKey: escapedLangName,
      kKeymanKeyboardVersionKey: kbVersion,
      kKeymanFontKey: jsFont
    ]

    NotificationCenter.default.post(name: .keymanKeyboardChanged, object: self,
                                    userInfo: [kKeymanKeyboardInfoKey: kbInfo])
    return true
  }

  public func setKeyboard(_ keyboard: [String: String]) -> Bool {
    return setKeyboard(withID: keyboard[kKeymanKeyboardIdKey]!,
                       languageID: keyboard[kKeymanLanguageIdKey]!,
                       keyboardName: keyboard[kKeymanKeyboardNameKey]!,
                       languageName: keyboard[kKeymanLanguageNameKey]!,
                       font: keyboard[kKeymanFontKey]!,
                       oskFont: keyboard[kKeymanOskFontKey])
  }

  private func jsFontString(_ font: String?) -> String {
    guard let font = font, !font.isEmpty && (font.contains(".ttf") || font.contains(".otf")) else {
      return "''"
    }

    guard font.hasSuffix(".ttf") || font.hasSuffix(".otf") else {
      return font
    }

    let familyName = "font_family_\(font.dropLast(4))"
    return """
    {"family":"\(familyName)","files":["\(font)"]}
    """
  }

  /// Adds a new keyboard to the list in the keyboard picker if it doesn't already exist.
  /// The keyboard must be downloaded (see `downloadKeyboard()`) or preloaded (see `preloadLanguageFile()`)
  ///
  /// - Parameters:
  ///   - isRTL: The writing direction is right to left
  ///   - isCustom: The keyboard is not provided by Keyman
  ///   - font: Custom font for text views as a JSON String (see keyboardsDictionary)
  ///   - oskFont: Font for the on-screen keyboard
  public func addKeyboard(withID keyboardID: String, languageID: String,
                          keyboardName: String, languageName: String,
                          isRTL: Bool, isCustom: Bool, font: String?, oskFont: String?) {
    // Check if keyboard file exists
    guard let kbVersion = latestKeyboardFileVersion(withID: keyboardID) else {
      kmLog("Could not add keyboard with ID: \(keyboardID) because the keyboard file does not exist",
        checkDebugPrinting: false)
      return
    }

    // Get keyboards list if it exists in user defaults, otherwise create a new one
    var userKeyboards = activeUserDefaults().array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String?]] ?? []

    var keyboard = [
      kKeymanLanguageIdKey: languageID,
      kKeymanLanguageNameKey: languageName,
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanKeyboardNameKey: keyboardName,
      kKeymanKeyboardVersionKey: kbVersion,
      kKeymanKeyboardRTLKey: isRTL ? "Y" : "N",
      kKeymanCustomKeyboardKey: isCustom ? "Y" : "N",
      kKeymanFontKey: font ?? "''"
    ]
    if let oskFont = oskFont {
      keyboard[kKeymanOskFontKey] = oskFont
    }

    // Update keyboard if it exists
    if let index = indexForUserKeyboard(withID: keyboardID, languageID: languageID) {
      userKeyboards[index] = keyboard
    } else {
      userKeyboards.append(keyboard)
    }

    let userData = activeUserDefaults()
    userData.set(userKeyboards, forKey: kKeymanUserKeyboardsListKey)
    userData.set([Date()], forKey: kKeymanSynchronizeSWKeyboardKey)
    userData.synchronize()
  }

  /// Removes a keyboard from the list in the keyboard picker if it exists.
  /// - Returns: The keyboard exists and was removed
  public func removeKeyboard(withID keyboardID: String, languageID: String) -> Bool {
    // Remove keyboard from the list if it exists
    if let index = indexForUserKeyboard(withID: keyboardID, languageID: languageID) {
      return removeKeyboard(at: index)
    }
    return false
  }

  /// Removes the keyboard at index from the keyboards list if it exists.
  /// - Returns: The keyboard exists and was removed
  public func removeKeyboard(at index: Int) -> Bool {
    let userData = activeUserDefaults()

    // If user defaults for keyboards list does not exist, do nothing.
    guard var userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) else {
      return false
    }

    guard index < userKeyboards.count else {
      return false
    }

    let kbDict = userKeyboards[index]
    userKeyboards.remove(at: index)
    userData.set(userKeyboards, forKey: kKeymanUserKeyboardsListKey)
    userData.set([Date()], forKey: kKeymanSynchronizeSWKeyboardKey)
    userData.synchronize()

    NotificationCenter.default.post(name: .keymanKeyboardRemoved, object: self,
                                    userInfo: [kKeymanKeyboardInfoKey: kbDict])
    return true
  }

  /// - Returns: Whether the keyboard exists in the user keyboards list
  public func userKeyboardExists(withID keyboardID: String?, languageID: String?) -> Bool {
    return indexForUserKeyboard(withID: keyboardID, languageID: languageID) != nil
  }

  /// - Returns: The index of the keyboard if it exist in user keyboards list
  public func indexForUserKeyboard(withID keyboardID: String?, languageID: String?) -> Int? {
    let userKeyboards = activeUserDefaults().array(forKey: kKeymanUserKeyboardsListKey)
    guard let keyboards = userKeyboards as? [[String: String]] else {
      return nil
    }
    return keyboards.index { kb in
      return keyboardID == kb[kKeymanKeyboardIdKey] && languageID == kb[kKeymanLanguageIdKey]
    }
  }

  /// - Returns: Info for the current keyboard, if a keyboard is set
  public var currentKeyboardInfo: [String: String]? {
    guard let keyboardID = keyboardID, let languageID = languageID else {
      return nil
    }

    let userKeyboards = activeUserDefaults().array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]]
    let userKeyboard = userKeyboards?.first { kb in
      return keyboardID == kb[kKeymanKeyboardIdKey] && languageID == kb[kKeymanLanguageIdKey]
    }
    if let userKeyboard = userKeyboard {
      return userKeyboard
    }

    let key = "\(languageID)_\(keyboardID)"
    if let kbInfo = keyboardsDictionary[key] {
      return kbInfo
    }

    return [
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanLanguageIdKey: languageID
    ]
  }

  /// Switch to the next keyboard.
  /// - Returns: Index of the newly selected keyboard.
  public func switchToNextKeyboard() -> Int? {
    guard let index = currentKeyboardIndex else {
      return nil
    }

    let userData = activeUserDefaults()
    guard let userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]] else {
      return index
    }
    if userKeyboards.isEmpty {
      return index
    }

    let newIndex = (index + 1) % userKeyboards.count
    setKeyboard(userKeyboards[newIndex])
    return newIndex
  }

  /// - Returns: Index of the current keyboard in the keyboard list.
  public var currentKeyboardIndex: Int? {
    let userData = activeUserDefaults()
    guard let userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]] else {
      return nil
    }
    return userKeyboards.index { isCurrentKeyboard($0) }
  }

  func isCurrentKeyboard(withID keyboardID: String?, languageID: String?) -> Bool {
    return self.keyboardID == keyboardID && self.languageID == languageID
  }

  func isCurrentKeyboard(_ keyboard: [String: Any]) -> Bool {
    let keyboardID = keyboard[kKeymanKeyboardIdKey] as? String
    let languageID = keyboard[kKeymanLanguageIdKey] as? String
    return keyboardID == self.keyboardID && languageID == self.languageID
  }

  /// - Returns: The font name for the given keyboard ID and languageID, or returns nil if
  ///   - The keyboard doesn't have a font
  ///   - The keyboard info is not available in the user keyboards list or in keyboardsDictionary
  public func fontNameForKeyboard(withID keyboardID: String, languageID: String) -> String? {
    let userData = activeUserDefaults()
    let userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey)

    if let userKeyboards = userKeyboards as? [[String: String]] {
      if let kb = userKeyboards.first(where: { isCurrentKeyboard($0) }) {
        let font = kb[kKeymanFontKey]
        if let fontFilename = self.fontFilename(fromJSONFont: font!) {
          return keymanFonts[fontFilename]?[kKeymanFontNameKey] as? String
        }
        return nil
      }
    }

    if let kb = keyboardsDictionary["\(languageID)_\(keyboardID)"] {
      let font = kb[kKeymanFontKey]
      if let fontFilename = self.fontFilename(fromJSONFont: font!) {
        return keymanFonts[fontFilename]?[kKeymanFontNameKey] as? String
      }
      return nil
    }

    return nil
  }

  /// - Returns: the OSK font name for the given keyboard ID and languageID, or returns nil if
  ///   - The keyboard doesn't have an OSK font
  ///   - The keyboard info is not available in the user keyboards list or in keyboardsDictionary
  func oskFontNameForKeyboard(withID keyboardID: String, languageID: String) -> String? {
    let userData = activeUserDefaults()
    let userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey)

    if let userKeyboards = userKeyboards as? [[String: String]] {
      if let kb = userKeyboards.first(where: { isCurrentKeyboard($0) }) {
        if let font = kb[kKeymanOskFontKey] {
          let fontFilename = self.fontFilename(fromJSONFont: font)
          return keymanFonts[fontFilename!]?[kKeymanFontNameKey] as? String
        }
      }
    }

    if let kb = keyboardsDictionary["\(languageID)_\(keyboardID)"] {
      if let font = kb[kKeymanOskFontKey] {
        let fontFilename = self.fontFilename(fromJSONFont: font)
        return keymanFonts[fontFilename!]?[kKeymanFontNameKey] as? String
      }
    }

    return nil
  }

  func isRTLKeyboard(withID keyboardID: String, languageID: String) -> Bool {
    // TODO: Refactor using a function that retrieves keyboard
    let userData = activeUserDefaults()
    if let userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]] {
      let keyboard = userKeyboards.first { keyboard in
        return keyboard[kKeymanKeyboardIdKey] == keyboardID && keyboard[kKeymanLanguageIdKey] == languageID
      }
      if let keyboard = keyboard {
        return keyboard[kKeymanKeyboardRTLKey] == "Y"
      }
    }
    let kbKey = "\(languageID)_\(keyboardID)"
    return keyboardsDictionary[kbKey]?[kKeymanKeyboardRTLKey] == "Y"
  }

  func keyboards(for index: Int) -> [[String: Any]]? {
    if index < languages.count {
      return languages[index][kKeymanLanguageKeyboardsKey] as? [[String: Any]]
    }
    return nil
  }

  func keyboardInfo(forLanguageIndex languageIndex: Int, keyboardIndex: Int) -> [String: Any]? {
    guard let keyboards = keyboards(for: languageIndex) else {
      return nil
    }
    if keyboardIndex < keyboards.count {
      return keyboards[keyboardIndex]
    }
    return nil
  }

  func fontFilename(fromJSONFont jsonFont: String) -> String? {
    if jsonFont.hasFontExtension {
      return jsonFont
    }

    let jsonObject: Any
    do {
      jsonObject = try JSONSerialization.jsonObject(with: jsonFont.data(using: String.Encoding.utf8)!,
                                                    options: .mutableContainers)
    } catch {
      kmLog("Error decoding font JSON object: \(error)", checkDebugPrinting: false)
      return nil
    }
    guard let fontDict = jsonObject as? [AnyHashable: Any] else {
      return nil
    }
    let fontFiles = fontDict[kKeymanFontFilesKey]
    if let fontFiles = fontFiles as? [String] {
      if let fontFile = fontFiles.first(where: { $0.hasFontExtension }) {
        return fontFile
      }
    } else if let fontFile = fontFiles as? String {
      if fontFile.hasFontExtension {
        return fontFile
      }
    }
    return nil
  }

  func jsFont(forKeyboardID kbID: String, languageID langID: String) -> String {
    let kbKey = "\(langID)_\(kbID)"
    let kbDict = keyboardsDictionary[kbKey]
    let jsFont = kbDict?[kKeymanFontKey]
    return jsFont ?? "''"
  }

  func jsOskFont(forKeyboardID kbID: String, languageID langID: String) -> String {
    let kbKey = "\(langID)_\(kbID)"
    let kbDict = keyboardsDictionary[kbKey]
    let jsOskFont = kbDict?[kKeymanOskFontKey]
    return jsOskFont ?? "''"
  }

  func jsFont(fromFontDictionary fontDict: [AnyHashable: Any]?) -> String {
    guard let fontDict = fontDict, !fontDict.isEmpty else {
      return "''"
    }

    let data: Data
    do {
      data = try JSONSerialization.data(withJSONObject: fontDict, options: [])
    } catch {
      kmLog("Failed to encode font dictionary as JSON: \(String(describing: fontDict))", checkDebugPrinting: false)
      return "''"
    }

    return String(data: data, encoding: .ascii)!
      .replacingOccurrences(of: kKeymanFontFilenameKey, with: kKeymanFontFilesKey)
      .replacingOccurrences(of: kKeymanFontSourceKey, with: kKeymanFontFilesKey)
  }

  // MARK: - Downloading keyboards

  /// Asynchronously fetches the dictionary of possible languages/keyboards to be displayed in the keyboard picker.
  /// If not called before the picker is shown, the dictionary will be fetched automatically.
  /// This method allows you to fetch the info in advance at a time that's appropriate for your app.
  /// See `NSNotification.Name+Notifications` for a list of relevant notifications.
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
    let url = URL(string: "\(kKeymanApiBaseURL)languages?device=\(deviceType)")!
    let userData = completionBlock.map { ["completionBlock": $0] }

    let request = HTTPDownloadRequest(url: url, downloadType: .downloadCachedData, userInfo: userData ?? [:])
    currentRequest = request
    sharedQueue.addRequest(request)
    sharedQueue.run()
  }

  /// Asynchronously fetches the .js file for the keyboard with given IDs.
  /// See `NSNotification+Notifications` for notification on success/failiure.
  /// - Parameters:
  ///   - isUpdate: Keep the keyboard files on failure
  public func downloadKeyboard(withID keyboardID: String, languageID: String, isUpdate: Bool) {
    let kbInfo = [
      kKeymanKeyboardIdKey: keyboardID,
      kKeymanLanguageIdKey: languageID
    ]

    guard downloadQueue == nil else {
      // Download queue is active.
      return
    }

    guard reachability.currentReachabilityStatus() != NotReachable else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboard: kbInfo, error: error)
      return
    }

    guard let keyboardsInfo = keyboardsInfo else {
      let message = "Keyboard info has not yet been fetched. Call fetchKeyboardsList() first."
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
      downloadFailed(forKeyboard: kbInfo, error: error)
      return
    }

    let kbKey = "\(languageID)_\(keyboardID)"
    guard let keyboardDict = keyboardsDictionary[kbKey] else {
      let error = NSError(domain: "Keyman", code: 0,
                        userInfo: [NSLocalizedDescriptionKey: "Keyboard not found with key: \(kbKey)"])
      downloadFailed(forKeyboard: kbInfo, error: error)
      return
    }

    let urlStr = keyboardsInfo[keyboardID]![kKeymanKeyboardURIKey]!
    let keyboardURL = URL(string: urlStr)!

    let kbFontStr = keyboardDict[kKeymanFontKey] ?? ""
    let kbOskFontStr = keyboardDict[kKeymanOskFontKey] ?? ""
    var kbFontURLs: [URL] = []
    do {
      kbFontURLs += try keyboardFontURLs(jsonString: kbFontStr)
      kbFontURLs += try keyboardFontURLs(jsonString: kbOskFontStr)
    } catch {
      downloadFailed(forKeyboard: kbInfo, error: error as NSError)
      return
    }

    // TODO: Better typing
    downloadQueue = HTTPDownloader(self)
    let commonUserData: [String: Any] = [
      kKeymanKeyboardInfoKey: kbInfo,
      kKeymanUpdateKey: isUpdate ? 1 : 0
    ]
    downloadQueue!.userInfo = commonUserData

    let kbVersion = keyboardDict[kKeymanKeyboardVersionKey]
    let keyboardPath = self.keyboardPath(forFilename: keyboardURL.lastPathComponent,
                                         keyboardVersion: kbVersion)

    var request = HTTPDownloadRequest(url: keyboardURL, userInfo: commonUserData)
    request.destinationFile = keyboardPath?.path
    request.tag = 0
    downloadQueue!.addRequest(request)

    for (i, url) in kbFontURLs.enumerated() {
      request = HTTPDownloadRequest(url: url, userInfo: commonUserData)
      request.destinationFile = fontPath(forFilename: url.lastPathComponent)?.path
      request.tag = i + 1
      downloadQueue!.addRequest(request)
    }
    downloadQueue!.run()
  }

  private func keyboardFontURLs(jsonString: String) throws -> [URL] {
    let font = try? JSONSerialization.jsonObject(with: jsonString.data(using: String.Encoding.utf8)!,
                                                 options: .mutableContainers)

    guard let fontObject = font as? [AnyHashable: Any] else {
      return []
    }

    guard let baseString = options[kKeymanFontBaseURIKey] as? String,
          let baseURL = URL(string: baseString) else {
      kmLog("Missing font base URL in options: \(options)", checkDebugPrinting: false)
      return []
    }

    let fontFiles = fontObject[kKeymanFontFilesKey]
    if let fontFiles = fontFiles as? [String] {
      return fontFiles.filter({ $0.hasFontExtension }).map({ baseURL.appendingPathComponent($0) })
    } else if let source = fontFiles as? String {
      if source.hasFontExtension {
        return [baseURL.appendingPathComponent(source)]
      }
      return []
    } else {
      let message = "Unexpected error: \(String(describing: fontFiles)) is not a valid type to be processed."
      throw NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
    }
  }

  private func urlOrThrow(string: String) throws -> URL {
    if let url = URL(string: string) {
      return url
    }
    throw NSError(domain: "Keyman", code: 0,
                  userInfo: [NSLocalizedDescriptionKey: "Invalid URL: \(string)"])
  }

  /// Downloads a custom keyboard from the URL
  /// - Parameters:
  ///   - jsonUrl: URL to a JSON description of the keyboard
  ///   - isDirect: The keyboard is downloaded directly instead of via Keyman Engine Cloud Services.
  ///     Should normally be false to permit caching and prevent overloading a target server.
  public func downloadKeyboard(from jsonUrl: URL, isDirect: Bool) {
    let kbInfo = [
      kKeymanKeyboardIdKey: "",
      kKeymanLanguageIdKey: "",
      kKeymanKeyboardNameKey: "",
      kKeymanLanguageNameKey: "",
      kKeymanCustomKeyboardKey: "Y"
    ]

    guard reachability.currentReachabilityStatus() != NotReachable else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "No connection"])
      downloadFailed(forKeyboard: kbInfo, error: error)
      return
    }

    let url: URL
    if isDirect {
      url = jsonUrl
    } else {
      let deviceParam = (UIDevice.current.userInterfaceIdiom == .phone) ? "iphone" : "ipad"
      let encodedUrl = jsonUrl.absoluteString.addingPercentEncoding(withAllowedCharacters: .urlPathAllowed)!
      url = URL(string: "\(kKeymanApiRemoteURL)\(encodedUrl)&device=\(deviceParam)")!
    }

    guard let data = try? Data(contentsOf: url) else {
      let error = NSError(domain: "Keyman", code: 0,
                        userInfo: [NSLocalizedDescriptionKey: "Failed to fetch JSON file"])
      downloadFailed(forKeyboard: kbInfo, error: error)
      return
    }

    let jsonObj: Any
    do {
      jsonObj = try JSONSerialization.jsonObject(with: data, options: .mutableContainers)
    } catch {
      downloadFailed(forKeyboard: kbInfo, error: error as NSError)
      return
    }

    guard let jsonDict = jsonObj as? [AnyHashable: Any] else {
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "The keyboard could not be installed"])
      downloadFailed(forKeyboard: kbInfo, error: error)
      return
    }

    downloadKeyboard(fromDictionary: jsonDict)
  }

  /// Downloads a custom keyboard from dictionary
  /// - Parameter jsonDict: Dictionary with the fields contained in a JSON keyboard file
  func downloadKeyboard(fromDictionary jsonDict: [AnyHashable: Any]) {

    // TODO: parse JSON in a more type-safe manner
    let options = jsonDict[kKeymanOptionsKey] as? [AnyHashable: Any]
    let kbBaseUri = options?[kKeymanKeyboardBaseURIKey] as? String
    let fontBaseUri = options?[kKeymanFontBaseURIKey] as? String

    guard let keyboard = jsonDict[kKeymanKeyboardKey] as? [AnyHashable: Any],
          let fileName = keyboard[kKeymanKeyboardFilenameKey] as? String,
          let kbID = keyboard[kKeymanIdKey] as? String,
          let kbName = keyboard[kKeymanNameKey] as? String,
    let languages = keyboard[kKeymanLanguagesKey] as? [[String: Any]] else {
        let error = NSError(domain: "Keyman", code: 0,
                            userInfo: [NSLocalizedDescriptionKey: "The keyboard could not be installed"])
        let emptyKeyboard = [
          kKeymanKeyboardIdKey: "",
          kKeymanLanguageIdKey: "",
          kKeymanKeyboardNameKey: "",
          kKeymanLanguageNameKey: "",
          kKeymanKeyboardVersionKey: "",
          kKeymanKeyboardRTLKey: "",
          kKeymanCustomKeyboardKey: "Y"
        ]
        downloadFailed(forKeyboard: emptyKeyboard, error: error)
        return
    }
    let kbVersion = keyboard[kKeymanKeyboardVersionKey] as? String ?? "1.0"
    let isRTL = keyboard[kKeymanKeyboardRTLKey] as? String ?? "N"
    let kbFont = keyboard[kKeymanFontKey] as? [AnyHashable: Any]
    let kbOskFont = keyboard[kKeymanOskFontKey] as? [AnyHashable: Any]

    let jsFont = self.jsFont(fromFontDictionary: kbFont)
    let jsOskFont = kbOskFont.map { self.jsFont(fromFontDictionary: $0) }

    let keyboardInfos = languages.map { language -> [String: String] in
      var info = [
        kKeymanKeyboardIdKey: kbID,
        kKeymanLanguageIdKey: language[kKeymanIdKey] as! String,
        kKeymanKeyboardNameKey: kbName,
        kKeymanLanguageNameKey: language[kKeymanNameKey] as! String,
        kKeymanKeyboardVersionKey: kbVersion,
        kKeymanKeyboardRTLKey: isRTL,
        kKeymanCustomKeyboardKey: "Y",
        kKeymanFontKey: jsFont
      ]
      if let jsOskFont = jsOskFont {
        info[kKeymanOskFontKey] = jsOskFont
      }
      return info
    }

    // TODO: Don't have the special case for a single keyboard info
    let kbInfo: [String: Any]
    if keyboardInfos.count == 1 {
      kbInfo = [kKeymanKeyboardInfoKey: keyboardInfos[0]]
    } else {
      kbInfo = [kKeymanKeyboardInfoKey: keyboardInfos]
    }

    if downloadQueue != nil {
      // Download queue is active.
      let error = NSError(domain: "Keyman", code: 0,
                          userInfo: [NSLocalizedDescriptionKey: "Download queue is busy"])
      downloadFailed(forKeyboard: kbInfo, error: error)
      return
    }

    if reachability.currentReachabilityStatus() == NotReachable {
      let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: "No internet connection"])
      downloadFailed(forKeyboard: kbInfo, error: error)
      return
    }

    let keyboardLocalPath = self.keyboardPath(forFilename: fileName, keyboardVersion: kbVersion)!
    // FIXME: Maybe change to use appendingPathComponent
    let keyboardURL = URL(string: kbBaseUri! + fileName)!
    var keyboardFontURLs: [URL] = []
    // TODO: Refactor duplicate with kbOskFont
    if let kbFont = kbFont, !kbFont.isEmpty {
      // Font filename is deprecated
      let fontSource = kbFont[kKeymanFontSourceKey] ?? kbFont[kKeymanFontFilenameKey]
      if let fontSource = fontSource as? [String] {
        keyboardFontURLs = fontSource.flatMap { source in
          if source.hasFontExtension {
            return URL(string: fontBaseUri! + source)
          } else {
            return nil
          }
        }
      } else if let fontSource = fontSource as? String {
        if fontSource.hasFontExtension {
          keyboardFontURLs = [URL(string: fontBaseUri! + fontSource)!]
        } else {
          keyboardFontURLs = []
        }
      } else {
        let message = "Unexpected error: \(String(describing: fontSource)) is not a valid type to be processed."
        let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
        downloadFailed(forKeyboard: kbInfo, error: error)
        return
      }
    }
    if let kbOskFont = kbOskFont, !kbOskFont.isEmpty {
      // Font filename is deprecated
      let fontSource = kbOskFont[kKeymanFontSourceKey] ?? kbOskFont[kKeymanFontFilenameKey]
      if let fontSource = fontSource as? [String] {
        keyboardFontURLs.append(contentsOf: fontSource.flatMap { source in
          if source.hasFontExtension {
            let url = URL(string: fontBaseUri! + source)!
            if !keyboardFontURLs.contains(url) {
              return url
            }
          }
          return nil
        })
      } else if let fontSource = fontSource as? String {
        if fontSource.hasFontExtension {
          let url = URL(string: fontBaseUri! + fontSource)!
          if !keyboardFontURLs.contains(url) {
            keyboardFontURLs.append(url)
          }
        }
      } else {
        let message = "Unexpected error: \(String(describing: fontSource)) is not a valid type to be processed."
        let error = NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
        downloadFailed(forKeyboard: kbInfo, error: error)
        return
      }
    }

    let isUpdate = latestKeyboardFileVersion(withID: kbID) != nil

    downloadQueue = HTTPDownloader.init(self)
    let commonUserData = [
      kKeymanKeyboardInfoKey: kbInfo[kKeymanKeyboardInfoKey]!,
      kKeymanUpdateKey: (isUpdate ? 1 : 0)
    ]
    downloadQueue!.userInfo = commonUserData

    var request = HTTPDownloadRequest(url: keyboardURL, userInfo: commonUserData)
    request.destinationFile = keyboardLocalPath.path
    request.tag = 0
    downloadQueue!.addRequest(request)
    for (i, url) in keyboardFontURLs.enumerated() {
      let fontPath = self.fontPath(forFilename: url.lastPathComponent)!
      // FIXME: Downloading from local path to local path?
      request = HTTPDownloadRequest(url: fontPath, userInfo: commonUserData)
      request.destinationFile = fontPath.path
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
    guard let latestVersion = latestKeyboardFileVersion(withID: keyboardID) else {
      return .needsDownload
    }

    // Check version
    guard let keyboardsInfo = keyboardsInfo else {
      return .upToDate
    }
    let kbVersion = keyboardsInfo[keyboardID]![kKeymanKeyboardVersionKey] ?? "1.0"
    if compareVersions(latestVersion, kbVersion) == .orderedDescending {
      return .upToDate
    } else {
      return .needsUpdate
    }
  }

  private func createKeyboardsInfo() {
    let kbBaseUri = options[kKeymanKeyboardBaseURIKey] as? String
    keyboardsInfo = [:]
    keyboardsDictionary = [:]
    for langDict in languages {
      let keyboards = langDict[kKeymanLanguageKeyboardsKey] as! [[String: Any]]
      for kbDict in keyboards {
        let langId = langDict[kKeymanIdKey] as? String
        let kbID = kbDict[kKeymanIdKey] as? String
        let kbVersion = kbDict[kKeymanKeyboardVersionKey] as? String ?? "1.0"
        let isRTL = kbDict[kKeymanKeyboardRTLKey] as? String ?? "N"

        if keyboardsInfo![kbID!] == nil {
          let kbUri = "\(kbBaseUri!)\(kbDict[kKeymanKeyboardFilenameKey]!)"
          var keyboard = [
            kKeymanKeyboardNameKey: (kbDict[kKeymanNameKey] as? String)!,
            kKeymanKeyboardVersionKey: kbVersion,
            kKeymanKeyboardRTLKey: isRTL,
            kKeymanKeyboardModifiedKey: (kbDict[kKeymanKeyboardModifiedKey] as? String)!,
            kKeymanKeyboardFileSizeKey: String((kbDict[kKeymanKeyboardFileSizeKey] as? Int)!),
            kKeymanKeyboardURIKey: kbUri
          ]
          if let font = kbDict[kKeymanFontKey] as? String {
            keyboard[kKeymanFontKey] = font
          }
          keyboardsInfo![kbID!] = keyboard
        }

        let dictKey = "\(langId!)_\(kbID!)"
        if keyboardsDictionary[dictKey] == nil {
          let oskFont = kbDict[kKeymanOskFontKey] as? [AnyHashable: Any]
          var dict = [
            kKeymanKeyboardIdKey: (kbDict[kKeymanIdKey] as? String)!,
            kKeymanLanguageIdKey: (langDict[kKeymanIdKey] as? String)!,
            kKeymanKeyboardNameKey: (kbDict[kKeymanNameKey] as? String)!,
            kKeymanLanguageNameKey: (langDict[kKeymanNameKey] as? String)!,
            kKeymanKeyboardVersionKey: kbVersion,
            kKeymanKeyboardRTLKey: isRTL,
            kKeymanFontKey: jsFont(fromFontDictionary: kbDict[kKeymanFontKey] as? [AnyHashable: Any])
          ]
          if let oskFont = oskFont {
            dict[kKeymanOskFontKey] = jsFont(fromFontDictionary: oskFont)
          }
          keyboardsDictionary[dictKey] = dict
        }
      }
    }
    updateUserKeyboardsList()
  }

  private func updateUserKeyboardsList() {
    if keyboardsDictionary.isEmpty {
      return
    }
    let userData = activeUserDefaults()

    let lastVersion = userData.string(forKey: kKeymanEngineVersionKey) ?? "1.0"
    if compareVersions(lastVersion, sdkVersion) == .orderedSame {
      return
    }
    userData.set(sdkVersion, forKey: kKeymanEngineVersionKey)

    guard var userKbList = userData.array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]] else {
      kmLog("No user keyboards to update", checkDebugPrinting: true)
      return
    }

    for i in userKbList.indices {
      let kbID = userKbList[i][kKeymanKeyboardIdKey]!
      let langID = userKbList[i][kKeymanLanguageIdKey]!
      if var kb = keyboardsDictionary["\(langID)_\(kbID)"] {
        kb[kKeymanKeyboardVersionKey] = latestKeyboardFileVersion(withID: kbID)
        kb[kKeymanCustomKeyboardKey] = "N"
        userKbList[i] = kb
      } else {
        var kb = userKbList[i]
        kb[kKeymanCustomKeyboardKey] = "Y"
        userKbList[i] = kb
      }
    }
    userData.set(userKbList, forKey: kKeymanUserKeyboardsListKey)
    userData.synchronize()
  }

  func keyboardIdForCurrentRequest() -> String? {
    if let currentRequest = currentRequest {
      let tmpStr = currentRequest.url.lastPathComponent
      if tmpStr.hasSuffix(".js") {
        return String(tmpStr.dropLast(3))
      }
      // TODO: Maybe don't return nil and just continue
      return nil
    } else if let downloadQueue = downloadQueue {
      let kbInfo = downloadQueue.userInfo[kKeymanKeyboardInfoKey]
      if let dict = kbInfo as? [String: String] {
        return dict[kKeymanKeyboardIdKey]
      } else if let array = kbInfo as? [[String: String]] {
        return array[0][kKeymanKeyboardIdKey]
      }
    }
    return nil
  }

  func downloadKeyboard(forLanguageIndex languageIndex: Int, keyboardIndex: Int, isUpdate: Bool) {
    guard let keyboard = keyboardInfo(forLanguageIndex: languageIndex, keyboardIndex: keyboardIndex) else {
      return
    }
    let kbID = keyboard[kKeymanIdKey] as! String
    let langID = languages[languageIndex][kKeymanIdKey] as? String
    downloadKeyboard(withID: kbID, languageID: langID!, isUpdate: isUpdate)
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

  public func downloadQueueFinished(_ queue: HTTPDownloader) {
    if isDebugPrintingOn {
      if let fontDir = activeFontDirectory()?.path {
        let contents = try? FileManager.default.contentsOfDirectory(atPath: fontDir)
        kmLog("Font Directory contents: \(String(describing: contents))", checkDebugPrinting: true)
      }
      if let langDir = activeLanguageDirectory()?.path {
        let contents = try? FileManager.default.contentsOfDirectory(atPath: langDir)
        kmLog("Language Directory contents: \(String(describing: contents))", checkDebugPrinting: true)
      }
    }
  }

  public func downloadRequestStarted(_ request: HTTPDownloadRequest) {
    // If we're downloading a new keyboard.
    // The extra check is there to filter out other potential request types in the future.
    if request.tag == 0 && request.typeCode == .downloadFile {
      NotificationCenter.default.post(name: .keymanKeyboardDownloadStarted, object: self, userInfo: request.userInfo)
    }
  }

  public func downloadRequestFinished(_ request: HTTPDownloadRequest) {
    switch request.typeCode {
    case .downloadFile:
      let kbInfo = request.userInfo[kKeymanKeyboardInfoKey]
      let keyboard: [String: String]
      if let kb = kbInfo as? [String: String] {
        keyboard = kb
      } else if let keyboards = kbInfo as? [[String: String]], !keyboards.isEmpty {
        keyboard = keyboards[0]
      } else {
        kmLog("downloadRequestFinished(): Unexpected userInfo: \(String(describing: kbInfo))",
          checkDebugPrinting: false)
        return
      }
      let kbID = keyboard[kKeymanKeyboardIdKey]!
      let kbVersion = keyboard[kKeymanKeyboardVersionKey]
      let isUpdate = (request.userInfo[kKeymanUpdateKey] as? Int)! != 0

      if let statusCode = request.responseStatusCode, statusCode == 200 {
        // The request has succeeded.
        if downloadQueue!.requestsCount == 0 {
          // Download queue finished.
          downloadQueue = nil
          registerCustomFonts()
          kmLog("Downloaded keyboard: \(kbID).", checkDebugPrinting: true)

          NotificationCenter.default.post(name: .keymanKeyboardDownloadCompleted, object: self,
                                          userInfo: request.userInfo)
          if isUpdate {
            shouldReloadKeyboard = true
            reloadKeyboard()
          }
          let userData = activeUserDefaults()
          userData.set([Date()], forKey: kKeymanSynchronizeSWKeyboardKey)
          userData.synchronize()
        }
      } else { // Possible request error (400 Bad Request, 404 Not Found, etc.)
        downloadQueue!.cancelAllOperations()
        downloadQueue = nil

        let errorMessage = "\(request.responseStatusMessage ?? ""): \(request.url)"
        let error = NSError(domain: "Keyman", code: 0,
                            userInfo: [NSLocalizedDescriptionKey: errorMessage])
        kmLog("Keyboard download failed: \(error).", checkDebugPrinting: true)

        if !isUpdate {
          let fileName = request.url.lastPathComponent
          if fileName.hasJavaScriptExtension {
            if let kbPath = keyboardPath(forFilename: fileName, keyboardVersion: kbVersion) {
              try? FileManager.default.removeItem(at: kbPath)
            }
          } else if fileName.hasFontExtension {
            // TODO: Why do we delete a keyboard with that name?
            if let kbPath = keyboardPath(forFilename: fileName, keyboardVersion: kbVersion) {
              try? FileManager.default.removeItem(at: kbPath)
            }
            if let fontPath = activeFontDirectory()?.appendingPathComponent(fileName) {
              try? FileManager.default.removeItem(at: fontPath)
            }
          }
        }
        downloadFailed(forKeyboard: request.userInfo, error: error)
      }
    case .downloadCachedData:
      if request == currentRequest {
        do {
          let responseDict = try JSONSerialization.jsonObject(with: request.rawResponseData!,
                                                              options: .mutableContainers) as? [AnyHashable: Any]
          options = responseDict?[kKeymanOptionsKey] as? [AnyHashable: Any] ?? [:]
          let unsortedLanguages = (responseDict?[kKeymanLanguagesKey] as? [AnyHashable: Any])?[kKeymanLanguagesKey]
            as? [[String: Any]]
          languages = unsortedLanguages!.sorted { a, b -> Bool in
            let aName = a[kKeymanNameKey] as! String
            let bName = b[kKeymanNameKey] as! String
            return aName.localizedCaseInsensitiveCompare(bName) == .orderedAscending
          }

          createKeyboardsInfo()
          kmLog("Request completed -- \(languages.count) languages.", checkDebugPrinting: true)
          currentRequest = nil

          if let completionBlock = request.userInfo["completionBlock"] as? FetchKeyboardsBlock {
            completionBlock(nil)
          }

          NotificationCenter.default.post(name: .keymanLanguagesUpdated, object: self)
        } catch {
          kmLog("Failed: \(error).", checkDebugPrinting: true)
          let error = NSError(domain: "Keyman", code: 0,
                              userInfo: [NSLocalizedDescriptionKey: error.localizedDescription])
          NotificationCenter.default.post(name: .keymanLanguagesDownloadFailed, object: self,
                                          userInfo: [NSUnderlyingErrorKey: error])
        }
      }
    }
  }

  public func downloadRequestFailed(_ request: HTTPDownloadRequest) {
    switch request.typeCode {
    case .downloadFile:
      downloadQueue = nil
      let error = request.error ?? NSError(domain: "Keyman", code: 0, userInfo: nil)

      kmLog("Keyboard download failed: \(error).", checkDebugPrinting: true)
      let kbInfo = request.userInfo[kKeymanKeyboardInfoKey]
      let keyboard: [String: String]
      if let kb = kbInfo as? [String: String] {
        keyboard = kb
      } else if let keyboards = kbInfo as? [[String: String]], !keyboards.isEmpty {
        keyboard = keyboards[0]
      } else {
        kmLog("downloadRequestFailed(): Unexpected userInfo: \(String(describing: kbInfo))",
          checkDebugPrinting: false)
        return
      }
      let kbID = keyboard[kKeymanKeyboardIdKey]!
      let kbVersion = keyboard[kKeymanKeyboardVersionKey]
      let isUpdate = (request.userInfo[kKeymanUpdateKey] as? Int)! != 0

      if !isUpdate {
        let fileManager = FileManager.default
        let fileName = request.url.lastPathComponent
        if fileName.hasJavaScriptExtension {
          if let kbPath = keyboardPath(forFilename: fileName, keyboardVersion: kbVersion) {
            try? fileManager.removeItem(at: kbPath)
          }
        }
        if fileName.hasFontExtension {
          // TODO: Check why this doesn't match the error case in downloadRequestFinished().
          if let kbPath = keyboardPath(forID: kbID, keyboardVersion: kbVersion) {
            try? fileManager.removeItem(at: kbPath)
          }
        }
      }
      downloadFailed(forKeyboard: request.userInfo, error: error as NSError)
    case .downloadCachedData:
      if request == currentRequest {
        let error = request.error!
        kmLog("Failed: \(error).", checkDebugPrinting: true)

        currentRequest = nil

        if let completionBlock = request.userInfo["completionBlock"] as? FetchKeyboardsBlock {
          completionBlock([NSUnderlyingErrorKey: error])
        }
        NotificationCenter.default.post(name: .keymanLanguagesDownloadFailed, object: self,
                                        userInfo: [NSUnderlyingErrorKey: error])
      }
    }
  }

  func downloadFailed(forKeyboard keyboardInfo: [AnyHashable: Any], error: NSError) {
    let kbInfo = keyboardInfo[kKeymanKeyboardInfoKey] ?? keyboardInfo
    NotificationCenter.default.post(name: .keymanKeyboardDownloadFailed, object: self,
                                    userInfo: [
                                      kKeymanKeyboardInfoKey: kbInfo,
                                      NSUnderlyingErrorKey: error
      ])
  }

  // MARK: - Loading custom keyboards

  private func preloadFile(srcUrl: URL, dstDir dirUrl: URL, shouldOverwrite: Bool) {
    let dstUrl = dirUrl.appendingPathComponent(srcUrl.lastPathComponent)
    do {
      if !FileManager.default.fileExists(atPath: dstUrl.path) {
        try FileManager.default.copyItem(at: srcUrl, to: dstUrl)
      } else if shouldOverwrite {
        try FileManager.default.removeItem(at: dstUrl)
        try FileManager.default.copyItem(at: srcUrl, to: dstUrl)
      } else {
        kmLog("File already exists at \(dstUrl) and not overwriting", checkDebugPrinting: false)
        return
      }
      addSkipBackupAttribute(to: dstUrl)
    } catch {
      kmLog("Error copying file: \(error)", checkDebugPrinting: false)
    }
  }

  /// Preloads a .js file for a language so that the keyboard is available without downloading.
  /// - Precondition:
  ///   - The .js filename must remain the same as when obtained from Keyman.
  ///   - The .js file must be bundled in your application.
  public func preloadLanguageFile(atPath languagePath: String, shouldOverwrite: Bool) {
    guard let languageDir = activeLanguageDirectory() else {
      kmLog("Could not find/create the Keyman language directory", checkDebugPrinting: false)
      return
    }
    preloadFile(srcUrl: URL.init(fileURLWithPath: languagePath),
                dstDir: languageDir,
                shouldOverwrite: shouldOverwrite)
  }

  /// Preloads a .ttf or .otf file to be available without downloading.
  /// - Precondition:
  ///   - The font file must be bundled in your application.
  /// - SeeAlso: `registerCustomFonts()`
  public func preloadFontFile(atPath fontPath: String, shouldOverwrite: Bool) {
    guard let fontDir = activeFontDirectory() else {
      kmLog("Could not find/create the Keyman font directory", checkDebugPrinting: false)
      return
    }
    preloadFile(srcUrl: URL.init(fileURLWithPath: fontPath),
                dstDir: fontDir,
                shouldOverwrite: shouldOverwrite)
  }

  /// Registers all new fonts found in the font path. Call this after you have preloaded all your font files
  /// with `preloadFontFile(atPath:shouldOverwrite:)`
  public func registerCustomFonts() {
    // TODO: Why is this separate from preloading?
    let directoryContents: [String]
    do {
      directoryContents = try FileManager.default.contentsOfDirectory(atPath: activeFontDirectory().path)
    } catch {
      kmLog("Failed to list font dir contents: \(error)", checkDebugPrinting: false)
      return
    }

    for fontFilename in directoryContents where fontFilename.hasFontExtension {
      if let fontInfo = keymanFonts[fontFilename] {
        if fontInfo[kKeymanFontRegisteredKey] as? Int != 0 {
          if let newFontInfo = registerFont(withFilename: fontFilename) {
            keymanFonts[fontFilename] = newFontInfo
          }
        }
      } else {
        if let fontInfo = registerFont(withFilename: fontFilename) {
          keymanFonts[fontFilename] = fontInfo
        }
      }
    }
  }

  /// Unregisters all registered fonts in the font path.
  public func unregisterCustomFonts() {
    let directoryContents: [String]
    do {
      directoryContents = try FileManager.default.contentsOfDirectory(atPath: activeFontDirectory().path)
    } catch {
      kmLog("Failed to list font dir contents: \(error)", checkDebugPrinting: false)
      return
    }

    for fontFilename in directoryContents where fontFilename.hasFontExtension {
      if var fontInfo = keymanFonts[fontFilename], fontInfo[kKeymanFontRegisteredKey] as? Int != 0 {
        fontInfo[kKeymanFontRegisteredKey] = 0
        keymanFonts[fontFilename] = fontInfo
      }
    }
  }

  private func registerFont(withFilename fontFilename: String) -> [String: Any]? {
    guard let fontURL = activeFontDirectory()?.appendingPathComponent(fontFilename),
      FileManager.default.fileExists(atPath: fontURL.path) else {
        return nil
    }

    guard let provider = CGDataProvider(url: fontURL as CFURL) else {
      kmLog("Failed to open \(fontURL)", checkDebugPrinting: false)
      return nil
    }
    guard let font = CGFont(provider) else {
      kmLog("Failed to read font at \(fontURL)", checkDebugPrinting: false)
      return nil
    }

    var didRegister: Bool = false
    let fontName = font.postScriptName! as String
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
    return [
      kKeymanFontNameKey: fontName,
      // TODO: Check if didRegister should be true if font exists
      kKeymanFontRegisteredKey: didRegister ? 1 : 0
    ]
  }

  private func unregisterFont(withFilename fontFilename: String) -> Bool {
    guard let fontURL = activeFontDirectory()?.appendingPathComponent(fontFilename),
      FileManager.default.fileExists(atPath: fontURL.path) else {
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
      let fontNames = UIFont.fontNames(forFamilyName: familyName)
      return fontNames.contains(fontName)
    }
  }

  // TODO: Use a logging library or have more than 2 log levels
  // Facilitates KeymanEngine internal logging.
  func kmLog(_ logStr: String, checkDebugPrinting: Bool) {
    if checkDebugPrinting && !isDebugPrintingOn {
      return
    }
    NSLog("%@", logStr)
    // TODO: Remove this
    NotificationCenter.default.post(name: .keymanDebugLog, object: self,
                                    userInfo: ["log": logStr])
  }

  // MARK: - File system and UserData management

  // Local file storage
  private func copyWebFilesToLibrary() {
    guard let libraryDirectory = activeKeymanDirectory() else {
      kmLog("Could not locate library directory! Could not copy Keyman files.", checkDebugPrinting: false)
      return
    }

    do {
      try copyFromBundle(resourceName: kKeymanFileName,
                         resourceExtension: kKeymanFileExtension,
                         dstDir: libraryDirectory)
      try copyFromBundle(resourceName: kKeymaniOSCodeFileName,
                         resourceExtension: nil,
                         dstDir: libraryDirectory)
      try copyFromBundle(resourceName: "\(kKeymanDefaultKeyboardID)-1.6",
                         resourceExtension: "js",
                         dstDir: activeLanguageDirectory())
      try copyFromBundle(resourceName: "DejaVuSans",
                         resourceExtension: "ttf",
                         dstDir: activeFontDirectory())
      try copyFromBundle(resourceName: "kmwosk",
                         resourceExtension: "css",
                         dstDir: libraryDirectory)
      try copyFromBundle(resourceName: "keymanweb-osk",
                         resourceExtension: "ttf",
                         dstDir: libraryDirectory)
    } catch {
      kmLog("copyWebFilesToLibrary: \(error)", checkDebugPrinting: false)
    }
  }

  private func copyFromBundle(resourceName: String, resourceExtension: String?, dstDir: URL?) throws {
    let filenameForLog = "\(resourceName)\(resourceExtension.map { ".\($0)" } ?? "")"
    guard let srcUrl = keymanBundle.url(forResource: resourceName, withExtension: resourceExtension) else {
      let message = "Could not locate \(filenameForLog) in the Keyman bundle for copying."
      throw NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
    }
    guard let dstDir = dstDir else {
      let message = "Destination directory for \(filenameForLog) is nil"
      throw NSError(domain: "Keyman", code: 0, userInfo: [NSLocalizedDescriptionKey: message])
    }
    let dstUrl = dstDir.appendingPathComponent(srcUrl.lastPathComponent)

    // FIXME: FileManager exceptions are swallowed.
    copyAndExcludeFromBackup(at: srcUrl, to: dstUrl)
  }

  private func compareFileModDates(_ a: String, _ b: String) -> ComparisonResult? {
    guard let aAttrs = try? FileManager.default.attributesOfItem(atPath: a),
          let bAttrs = try? FileManager.default.attributesOfItem(atPath: b),
          let aModDate = aAttrs[.modificationDate] as? Date,
          let bModDate = bAttrs[.modificationDate] as? Date else {
      return nil
    }
    if aModDate > bModDate {
      return .orderedDescending
    }
    if aModDate < bModDate {
      return .orderedAscending
    }
    return .orderedSame
  }

  func renameOldKeyboardFiles() {
    let fileManager = FileManager.default
    guard let langDir = activeLanguageDirectory(),
          let dirContents = try? fileManager.contentsOfDirectory(atPath: langDir.path) else {
      kmLog("renameOldKeyboardFiles(): Failed to list language directory", checkDebugPrinting: false)
      return
    }

    for filename in dirContents {
      if !filename.contains("-") {
        if filename == "us.js" {
          let fileUrl = langDir.appendingPathComponent(filename)
          try? fileManager.removeItem(at: fileUrl)

          let userData = activeUserDefaults()
          let curKB = userData.object(forKey: kKeymanUserCurrentKeyboardKey) as? [AnyHashable: Any]
          if curKB?[kKeymanKeyboardIdKey] as? String == "us" {
            userData.removeObject(forKey: kKeymanUserCurrentKeyboardKey)
            userData.synchronize()
          }
        } else {
          let newFilename = "\(filename.dropLast(3))-1.0.js"
          let fileUrl = langDir.appendingPathComponent(filename)
          let newFileUrl = langDir.appendingPathComponent(newFilename)
          try? fileManager.moveItem(at: fileUrl, to: newFileUrl)
        }
      } else {
        let oldFilename = "\(filename[filename.startIndex..<filename.index(of: "-")!]).js"
        let oldFileUrl = langDir.appendingPathComponent(oldFilename)
        try? fileManager.removeItem(at: oldFileUrl)
      }
    }
  }

  // TODO: Consider making these lazy vars
  // FIXME: Check for errors when creating directory
  private func createSubdirectory(baseDir: URL?, name: String) -> URL? {
    guard let baseDir = baseDir else {
      return nil
    }
    let newDir = baseDir.appendingPathComponent(name)
    try? FileManager.default.createDirectory(at: newDir,
                                             withIntermediateDirectories: true,
                                             attributes: nil)
    return newDir
  }

  private func defaultKeymanDirectory() -> URL? {
    let paths = NSSearchPathForDirectoriesInDomains(.libraryDirectory, .userDomainMask, true)
    if paths.isEmpty {
      return nil
    }
    return createSubdirectory(baseDir: URL(fileURLWithPath: paths[0]), name: "keyman")
  }

  private func defaultLanguageDirectory() -> URL? {
    return createSubdirectory(baseDir: defaultKeymanDirectory(), name: "languages")
  }

  private func defaultFontDirectory() -> URL? {
    return createSubdirectory(baseDir: defaultKeymanDirectory(), name: "fonts")
  }

  var sharedContainerURL: URL? {
    guard let groupID = Manager.applicationGroupIdentifier else {
      kmLog("applicationGroupIdentifier is unset", checkDebugPrinting: false)
      return nil
    }
    return FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: groupID)
  }

  func sharedKeymanDirectory() -> URL? {
    return createSubdirectory(baseDir: sharedContainerURL, name: "keyman")
  }

  func sharedLanguageDirectory() -> URL? {
    return createSubdirectory(baseDir: sharedKeymanDirectory(), name: "languages")
  }

  func sharedFontDirectory() -> URL? {
    return createSubdirectory(baseDir: sharedKeymanDirectory(), name: "fonts")
  }

  func activeKeymanDirectory() -> URL! {
    return canAccessSharedContainer() ? sharedKeymanDirectory() : defaultKeymanDirectory()
  }

  func activeLanguageDirectory() -> URL! {
    return canAccessSharedContainer() ? sharedLanguageDirectory() : defaultLanguageDirectory()
  }

  func activeFontDirectory() -> URL! {
    return canAccessSharedContainer() ? sharedFontDirectory() : defaultFontDirectory()
  }

  func activeUserDefaults() -> UserDefaults {
    return canAccessSharedContainer() ? sharedUserDefaults! : UserDefaults.standard
  }

  var sharedUserDefaults: UserDefaults? {
    guard let groupID = Manager.applicationGroupIdentifier else {
      kmLog("applicationGroupIdentifier is unset", checkDebugPrinting: false)
      return nil
    }
    return UserDefaults(suiteName: groupID)
  }

  func canAccessSharedContainer() -> Bool {
    guard let sharedKeymanDir = sharedKeymanDirectory() else {
      return false
    }
    if !isSystemKeyboard {
      return true
    }
    let keymanFile = sharedKeymanDir.appendingPathComponent(kKeymanFullFileName)
    return FileManager.default.fileExists(atPath: keymanFile.path)
  }

  private func copyUserDefaultsToSharedContainer() {
    guard let sharedUserData = sharedUserDefaults else {
      return
    }
    let defaultUserData = UserDefaults.standard
    let keysToCopy = [kKeymanUserKeyboardsListKey, kKeymanUserCurrentKeyboardKey,
                      kKeymanEngineVersionKey, kKeymanKeyboardPickerDisplayedKey]
    for key in keysToCopy {
      if sharedUserData.object(forKey: key) == nil {
        sharedUserData.set(defaultUserData.object(forKey: key), forKey: key)
      }
    }
    sharedUserData.synchronize()
  }

  private func copyUserDefaultsFromSharedContainer() {
    guard let sharedUserData = sharedUserDefaults else {
      return
    }
    let defaultUserData = UserDefaults.standard
    let keysToCopy = [kKeymanUserKeyboardsListKey, kKeymanEngineVersionKey]
    for key in keysToCopy {
      if sharedUserData.object(forKey: key) != nil {
        defaultUserData.set(sharedUserData.object(forKey: key), forKey: key)
      }
    }
    defaultUserData.synchronize()
  }

  private func addSkipBackupAttribute(to url: URL) -> Bool {
    var url = url
    assert(FileManager.default.fileExists(atPath: url.path))
    var resourceValues = URLResourceValues()
    resourceValues.isExcludedFromBackup = true
    do {
      // Writes values to the backing store. It is not only mutating the URL in memory.
      try url.setResourceValues(resourceValues)
      return true
    } catch {
      kmLog("Error excluding \(url) from backup \(error)", checkDebugPrinting: false)
      return false
    }
  }

  private func copyAndExcludeFromBackup(at src: URL, to dst: URL) -> Bool {
    let fm = FileManager.default

    var isDirectory: ObjCBool = false
    let fileExists = fm.fileExists(atPath: src.path, isDirectory: &isDirectory)

    if !fileExists || isDirectory.boolValue {
      return false
    }

    // copy if destination does not exist or replace if source is newer
    do {
      if !fm.fileExists(atPath: dst.path) {
        try fm.copyItem(at: src, to: dst)
      } else if compareFileModDates(src.path, dst.path) == .orderedDescending {
        try fm.removeItem(at: dst)
        try fm.copyItem(at: src, to: dst)
      } else {
        return false
      }
    } catch {
      kmLog("copyAndExcludeFromBackup: \(error)", checkDebugPrinting: false)
      return false
    }

    addSkipBackupAttribute(to: dst)
    return true
  }

  private func copyDirectoryContents(at srcDir: URL?, to dstDir: URL?) throws {
    guard let srcDir = srcDir,
      let dstDir = dstDir else {
        return
    }
    let srcContents = try FileManager.default.contentsOfDirectory(at: srcDir, includingPropertiesForKeys: [])
    for srcFile in srcContents {
      copyAndExcludeFromBackup(at: srcFile, to: dstDir.appendingPathComponent(srcFile.lastPathComponent))
    }
  }

  private func copyKeymanFilesToSharedContainer() -> Bool {
    do {
      try copyDirectoryContents(at: defaultKeymanDirectory(), to: sharedKeymanDirectory())
      try copyDirectoryContents(at: defaultLanguageDirectory(), to: sharedLanguageDirectory())
      try copyDirectoryContents(at: defaultFontDirectory(), to: sharedFontDirectory())
      return true
    } catch {
      kmLog("copyKeymanFilesToSharedContainer(): \(error)", checkDebugPrinting: false)
      return false
    }
  }

  private func copyKeymanFilesFromSharedContainer() -> Bool {
    do {
      try copyDirectoryContents(at: sharedKeymanDirectory(), to: defaultKeymanDirectory())
      try copyDirectoryContents(at: sharedLanguageDirectory(), to: defaultLanguageDirectory())
      try copyDirectoryContents(at: sharedFontDirectory(), to: defaultFontDirectory())
    } catch {
      kmLog("copyKeymanFilesFromSharedContainer(): \(error)", checkDebugPrinting: false)
      return false
    }
    registerCustomFonts()
    return true
  }

  private func copyKeymanFilesToTemp() -> Bool {
    let tempKeymanDir = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("keyman")
    let tempLangDir = tempKeymanDir.appendingPathComponent("languages")
    let tempFontDir = tempKeymanDir.appendingPathComponent("fonts")

    do {
      try FileManager.default.createDirectory(at: tempKeymanDir, withIntermediateDirectories: true, attributes: nil)
      try FileManager.default.createDirectory(at: tempLangDir, withIntermediateDirectories: true, attributes: nil)
      try FileManager.default.createDirectory(at: tempFontDir, withIntermediateDirectories: true, attributes: nil)
      try copyDirectoryContents(at: activeKeymanDirectory(), to: tempKeymanDir)
      try copyDirectoryContents(at: activeLanguageDirectory(), to: tempLangDir)
      try copyDirectoryContents(at: activeFontDirectory(), to: tempFontDir)
    } catch {
      kmLog("copyKeymanFilesToTemp(): \(error)", checkDebugPrinting: false)
      return false
    }
    return true
  }

  private var usingTempFolder: Bool {
    if #available(iOS 9.0, *) {
      return false
    }
    return true
  }

  // FIXME: The check for empty filename, etc was removed. Check whether that needs to be added back.
  private func keyboardPath(forID keyboardID: String, keyboardVersion: String?) -> URL? {
    var keyboardVersion = keyboardVersion
    if keyboardVersion == nil {
      keyboardVersion = latestKeyboardFileVersion(withID: keyboardID)
    }
    guard let version = keyboardVersion else {
      return nil
    }
    return activeLanguageDirectory()?.appendingPathComponent("\(keyboardID)-\(version).js")
  }

  func keyboardPath(forFilename filename: String, keyboardVersion: String?) -> URL? {
    if !filename.contains("-") {
      let name = "\(filename.dropLast(3))-\(keyboardVersion ?? "1.0").js"
      return activeLanguageDirectory()?.appendingPathComponent(name)
    }
    return activeLanguageDirectory()?.appendingPathComponent(filename)
  }

  func fontPath(forFilename filename: String) -> URL? {
    return activeFontDirectory()?.appendingPathComponent(filename)
  }

  func latestKeyboardFileVersion(withID keyboardID: String) -> String? {
    guard let langDirPath = activeLanguageDirectory()?.path else {
      return nil
    }
    guard let dirContents = try? FileManager.default.contentsOfDirectory(atPath: langDirPath) else {
      return nil
    }

    var latestVersion: String?
    for filename in dirContents where filename.hasPrefix("\(keyboardID)-") && filename.hasJavaScriptExtension {
      let dashRange = filename.range(of: "-", options: .backwards)!
      let extensionRange = filename.range(of: ".js", options: .backwards)!
      let version = String(filename[dashRange.upperBound..<extensionRange.lowerBound])

      if let previousMax = latestVersion {
        if let result = compareVersions(previousMax, version), result == .orderedAscending {
          latestVersion = previousMax
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
      guard let val2 = Int(component2), val1 >= 0 else {
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
    let userData = activeUserDefaults()
    guard var userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]] else {
      return
    }

    // Set version in user keyboards list
    for i in userKeyboards.indices {
      var kb = userKeyboards[i]
      if kbID == kb[kKeymanKeyboardIdKey] {
        kb[kKeymanKeyboardVersionKey] = kbVersion
        userKeyboards[i] = kb
      }
    }
    userData.set(userKeyboards, forKey: kKeymanUserKeyboardsListKey)
    userData.synchronize()

    // Set version for current keyboard
    // TODO: Move this UserDefaults into a function
    let currentUserData = isSystemKeyboard ? UserDefaults.standard : activeUserDefaults()
    if var userKb = currentUserData.object(forKey: kKeymanUserCurrentKeyboardKey) as? [String: String] {
      if kbID == userKb[kKeymanKeyboardIdKey] {
        userKb[kKeymanKeyboardVersionKey] = kbVersion
        currentUserData.set(userKb, forKey: kKeymanUserCurrentKeyboardKey)
        currentUserData.synchronize()
      }
    }
  }

  func synchronizeSWKeyboard() {
    copyUserDefaultsFromSharedContainer()
    copyKeymanFilesFromSharedContainer()
  }

  // MARK: - View management

  public var keyboardHeight: CGFloat {
    let isPortrait: Bool
    if isSystemKeyboard {
      isPortrait = KeymanInputViewController.isPortrait
    } else {
      isPortrait = UIDevice.current.orientation.isPortrait
    }

    if UIDevice.current.userInterfaceIdiom == .pad {
      if isPortrait {
        return isSystemKeyboard ? kKeymanPadPortraitSystemKeyboardHeight : kKeymanPadPortraitInAppKeyboardHeight
      } else {
        return isSystemKeyboard ? kKeymanPadLandscapeSystemKeyboardHeight
          : kKeymanPadLandscapeInAppKeyboardHeight
      }
    } else {
      if isPortrait {
        return isSystemKeyboard ? kKeymanPhonePortraitSystemKeyboardHeight
          : kKeymanPhonePortraitInAppKeyboardHeight
      } else {
        return isSystemKeyboard ? kKeymanPhoneLandscapeSystemKeyboardHeight
          : kKeymanPhoneLandscapeInAppKeyboardHeight
      }
    }
  }

  func keyboardHeight(with orientation: UIInterfaceOrientation) -> CGFloat {
    if UIDevice.current.userInterfaceIdiom == .pad {
      if orientation.isPortrait {
        return isSystemKeyboard ? kKeymanPadPortraitSystemKeyboardHeight
          : kKeymanPadPortraitInAppKeyboardHeight
      } else {
        return isSystemKeyboard ? kKeymanPadLandscapeSystemKeyboardHeight
          : kKeymanPadLandscapeInAppKeyboardHeight
      }
    } else {
      if orientation.isPortrait {
        return isSystemKeyboard ? kKeymanPhonePortraitSystemKeyboardHeight
          : kKeymanPhonePortraitInAppKeyboardHeight
      } else {
        return isSystemKeyboard ? kKeymanPhoneLandscapeSystemKeyboardHeight
          : kKeymanPhoneLandscapeInAppKeyboardHeight
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
    // FIXME: Indicate failure to load URL
    if #available(iOS 9.0, *) {
      if let url = activeKeymanDirectory()?.appendingPathComponent(kKeymanFullFileName) {
        view.loadFileURL(url, allowingReadAccessTo: url.deletingLastPathComponent())
      }
    } else {
      // Copy Keyman files to the temp folder and load from there
      if copyKeymanFilesToTemp() {
        let url = URL(fileURLWithPath: NSTemporaryDirectory())
          .appendingPathComponent("keyman")
          .appendingPathComponent(kKeymanFullFileName)
        view.load(URLRequest(url: url, cachePolicy: .reloadIgnoringCacheData, timeoutInterval: 60.0))
      }
    }
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
      NotificationCenter.default.post(name: .keymanSubKeysMenuDismissed, object: self, userInfo: nil)
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
  /// KeymanTextView/KeymanTextField to enable/disable the keyboard picker
  public func showKeyboardPicker(in viewController: UIViewController, shouldAddKeyboard: Bool) {
    let vc = KeyboardPickerViewController()
    let nc = UINavigationController(rootViewController: vc)
    nc.modalTransitionStyle = .coverVertical
    nc.modalPresentationStyle = .pageSheet
    viewController.present(nc, animated: true) {() -> Void in
      if shouldAddKeyboard {
        vc.showAddKeyboard()
      } else {
        let userData = self.activeUserDefaults()
        userData.set(true, forKey: kKeymanKeyboardPickerDisplayedKey)
        userData.synchronize()
        self.isKeymanHelpOn = false
      }
    }
  }

  public func dismissKeyboardPicker(_ viewController: UIViewController) {
    viewController.dismiss(animated: true)
    if shouldReloadKeyboard {
      reloadKeyboard()
    }
    NotificationCenter.default.post(name: .keymanKeyboardPickerDismissed, object: self)
  }

  func reloadKeyboard() {
    // FIXME: Duplicated elsewhere
    if #available(iOS 9.0, *) {
      guard let codeURL = activeKeymanDirectory()?.appendingPathComponent(kKeymanFullFileName) else {
        return
      }
      inputView.loadFileURL(codeURL, allowingReadAccessTo: codeURL.deletingLastPathComponent())
    } else {
      // Copy Keyman files to the temp folder and load from there
      if copyKeymanFilesToTemp() {
        let codeURL = URL(fileURLWithPath: NSTemporaryDirectory())
          .appendingPathComponent("keyman")
          .appendingPathComponent(kKeymanFullFileName)
        inputView.load(URLRequest(url: codeURL, cachePolicy: .reloadIgnoringCacheData, timeoutInterval: 60.0))
      }
    }
  }

  @objc func resetKeyboard() {
    keyboardID = nil
    languageID = nil

    let userKeyboards = activeUserDefaults().array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]]
    if let index = indexForUserKeyboard(withID: keyboardID, languageID: languageID) {
      setKeyboard(userKeyboards![index])
    } else if let userKeyboards = userKeyboards, !userKeyboards.isEmpty {
      setKeyboard(userKeyboards[0])
    } else {
      setKeyboard(withID: kKeymanDefaultKeyboardID, languageID: kKeymanDefaultLanguageID,
                  keyboardName: kKeymanDefaultKeyboardName, languageName: kKeymanDefaultLanguageName,
                  font: kKeymanDefaultKeyboardFont, oskFont: nil)
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
      isPortrait = KeymanInputViewController.isPortrait
    } else {
      isPortrait = UIApplication.shared.statusBarOrientation.isPortrait
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
    helpText.text = kKeymanKeyboardChangeHelpText
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

  // TODO: Remove dependence on this since we have super retina screens.
  var retinaScreen: Bool {
    return UIScreen.main.scale == 2.0
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
    guard url.lastPathComponent == kKeymanFullFileName && (url.fragment?.isEmpty ?? true) else {
      return
    }

    kmLog("Loaded keyboard.", checkDebugPrinting: true)
    resizeKeyboard()
    let deviceType = (UIDevice.current.userInterfaceIdiom == .phone) ? "AppleMobile" : "AppleTablet"
    webView.evaluateJavaScript("setDeviceType('\(deviceType)');", completionHandler: nil)
    if (keyboardID == nil || languageID == nil) && !shouldReloadKeyboard {
      var newKb = kKeymanDefaultKeyboard
      let userData = isSystemKeyboard ? UserDefaults.standard : activeUserDefaults()
      if let currentKb = userData.dictionary(forKey: kKeymanUserCurrentKeyboardKey) as? [String: String] {
        let kbID = currentKb[kKeymanKeyboardIdKey]
        let langID = currentKb[kKeymanLanguageIdKey]
        if userKeyboardExists(withID: kbID, languageID: langID) {
          newKb = currentKb
          if let kbName = currentKb[kKeymanKeyboardNameKey]?.replacingOccurrences(of: "\\'", with: "\'") {
            newKb[kKeymanKeyboardNameKey] = kbName
          }
          if let langName = currentKb[kKeymanLanguageNameKey]?.replacingOccurrences(of: "\\'", with: "\'") {
            newKb[kKeymanLanguageNameKey] = langName
          }
        }
      } else if let userKbs = activeUserDefaults().array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]],
        !userKbs.isEmpty {
        newKb = userKbs[0]
      }
      setKeyboard(newKb)
    }

    let kbInfo = [kKeymanKeyboardIdKey: keyboardID, kKeymanLanguageIdKey: languageID]

    NotificationCenter.default.post(name: .keymanKeyboardLoaded, object: self,
                                    userInfo: [kKeymanKeyboardInfoKey: kbInfo])
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
        userData.set(true, forKey: kKeymanKeyboardPickerDisplayedKey)
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
        NotificationCenter.default.post(name: .keymanSubKeysMenuDismissed, object: self, userInfo: nil)
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
    NotificationCenter.default.post(name: .keymanSubKeysMenuWillShow, object: self, userInfo: nil)
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
  // TODO: Manager should not have InputViewController methods. Move this into KeymanInputViewController.
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

    let activeUserDef = activeUserDefaults()
    let standardUserDef = UserDefaults.standard
    let activeDate = (activeUserDef.object(forKey: kKeymanSynchronizeSWKeyboardKey) as? [Date])?[safe: 0]
    let standardDate = (standardUserDef.object(forKey: kKeymanSynchronizeSWKeyboardKey) as? [Date])?[safe: 0]

    let shouldSynchronize: Bool
    if let standardDate = standardDate,
       let activeDate = activeDate {
      shouldSynchronize = standardDate != activeDate
    } else if activeDate == nil {
      shouldSynchronize = false
    } else {
      shouldSynchronize = true
    }

    if (!didSynchronize || shouldSynchronize) && canAccessSharedContainer() {
      synchronizeSWKeyboard()
      if keyboardID != nil && languageID != nil {
        shouldReloadKeyboard = true
        reloadKeyboard()
      }
      didSynchronize = true
      standardUserDef.set(activeUserDef.object(forKey: kKeymanSynchronizeSWKeyboardKey),
                          forKey: kKeymanSynchronizeSWKeyboardKey)
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

  func showKeyboardMenu(_ ic: KeymanInputViewController, closeButtonTitle: String) {
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
