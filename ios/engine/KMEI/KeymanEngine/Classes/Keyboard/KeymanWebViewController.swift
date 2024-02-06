//
//  KeymanWebViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-31.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit
import WebKit
import AudioToolbox
import Sentry
import os.log

private let keyboardChangeHelpText = NSLocalizedString("keyboard-help-change", bundle: engineBundle, comment: "")

private let subKeyColor = Colors.popupKey
private let subKeyColorHighlighted = Colors.popupKeyHighlighted

// We subclass for one critical reason - by default, WebViews may become first responder...
// a detail that is really, REALLY bad for a WebView in a keyboard.
//
// Minor reference here: https://stackoverflow.com/questions/39829863/can-a-uiwebview-handle-user-interaction-without-becoming-first-responder
//
// Confirmed issue existed within app during workaround for https://github.com/keymanapp/keyman/issues/2716
class KeymanWebView: WKWebView {
  override public var canBecomeFirstResponder: Bool {
    return false;
  }

  override public func becomeFirstResponder() -> Bool {
    return false;
  }
}

// MARK: - UIViewController
class KeymanWebViewController: UIViewController {
  let storage: Storage
  weak var delegate: KeymanWebDelegate?
  private var useSpecialFont = false
  private var userContentController = WKUserContentController()
  private let keymanWebViewName: String = "keyman"

  // Views
  var webView: KeymanWebView?
  var activeModel: Bool = false
  private var helpBubbleView: PopoverView?
  private var keyPreviewView: KeyPreviewView?
  private var subKeysView: SubKeysView?
  private var keyboardMenuView: KeyboardMenuView?

  // Arrays
  private var subKeyIDs: [String] = []
  private var subKeyTexts: [String] = []
  private var subKeys: [UIButton] = []

  private var subKeyAnchor = CGRect.zero

  /// Stores the keyboard view's current size.
  private var kbSize: CGSize = CGSize.zero

  /// Stores the current image for use by the Banner
  /// when predictive text is not active
  private var bannerImgPath: String = ""

  var shouldReload: Bool = true
  var isLoading: Bool = false

  private var currentText: String = ""
  private var currentCursorRange: NSRange? = nil

  init(storage: Storage) {
    self.storage = storage
    super.init(nibName: nil, bundle: nil)

    _ = view
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  @objc func fixLayout() {
    view.setNeedsLayout()
    view.layoutIfNeeded()
  }

  override func viewWillLayoutSubviews() {
    // This method is called automatically during layout correction by iOS.
    // It also has access to correct `view.bounds.size` values, unlike viewDidAppear.
    // As a result, it's the correct place to perform OSK size adjustments.
    //
    // Problem - this is ALSO called automatically upon any touch-based interaction with the OSK!  (Why!?)
    // The `keyboardSize` property will filter out any such redundant size-change requests to prevent issues
    // that would otherwise arise.  (Important event handlers can trigger for the original OSK instance
    // after it has been replaced by KMW's OSK resizing operation.)

    keyboardSize = view.bounds.size
  }

  open override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
    super.viewWillTransition(to: size, with: coordinator)

    if Manager.shared.isKeymanHelpOn {
      showHelpBubble(afterDelay: 1.5)
    }

    coordinator.animateAlongsideTransition(in: nil, animation: {
      _ in
        self.fixLayout()
    }, completion: {
      _ in
      // When going from landscape to portrait, the value is often not properly set until the end of the call chain.
      // A simple, ultra-short timer allows us to quickly rectify the value in these cases to correct the keyboard.
      Timer.scheduledTimer(timeInterval: 0.01, target: self, selector: #selector(self.fixLayout), userInfo: nil, repeats: false)
    })
  }

  override func loadView() {
    let config = WKWebViewConfiguration()
    let prefs = WKPreferences()
    prefs.javaScriptEnabled = true
    config.preferences = prefs
    config.suppressesIncrementalRendering = false
    config.userContentController = self.userContentController

    webView = KeymanWebView(frame: CGRect(origin: .zero, size: keyboardSize), configuration: config)
    webView!.isOpaque = false
    webView!.translatesAutoresizingMaskIntoConstraints = false
    webView!.backgroundColor = UIColor.clear
    webView!.navigationDelegate = self
    webView!.scrollView.isScrollEnabled = false

    view = webView

    // Set UILongPressGestureRecognizer to show sub keys
//    let hold = UILongPressGestureRecognizer(target: self, action: #selector(self.holdAction))
//    hold.minimumPressDuration = 0.5
//    hold.delegate = self
//    view.addGestureRecognizer(hold)

    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillShow),
                                           name: UIResponder.keyboardWillShowNotification, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillHide),
                                           name: UIResponder.keyboardWillHideNotification, object: nil)

    reloadKeyboard()
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    self.userContentController.add(self, name: keymanWebViewName)
  }

  // Very useful for immediately adjusting the WebView's properties upon loading.
  override func viewDidAppear(_ animated: Bool) {
    fixLayout()

    // Initialize the keyboard's size/scale.  In iOS 13 (at least), the system
    // keyboard's width will be set at this stage, but not in viewWillAppear.
    keyboardSize = view.bounds.size
  }

  override func viewWillDisappear(_ animated: Bool) {
    self.userContentController.removeScriptMessageHandler(forName: keymanWebViewName)
  }
}

// MARK: - JavaScript functions
extension KeymanWebViewController {
  func languageMenuPosition(_ completion: @escaping (CGRect) -> Void) {
    webView!.evaluateJavaScript("langMenuPos();") { result, _ in
      guard let result = result as? String, !result.isEmpty else {
        return
      }
      let components = result.components(separatedBy: ",")
      let x = CGFloat(Float(components[0])!)
      let y = CGFloat(Float(components[1])!)
      let w = CGFloat(Float(components[2])!)
      let h = CGFloat(Float(components[3])!)
      completion(KeymanWebViewController.keyFrame(x: x, y: y, w: w, h: h))
    }
  }

  // FIXME: text is unused in the JS
  func executePopupKey(id: String, text: String) {
    // Text must be checked for ', ", and \ characters; they must be escaped properly!
    do {
      let encodingArray = [ text ];
      let jsonString = try String(data: JSONSerialization.data(withJSONObject: encodingArray), encoding: .utf8)!
      let start = jsonString.index(jsonString.startIndex, offsetBy: 2)
      let end = jsonString.index(jsonString.endIndex, offsetBy: -2)
      let escapedText = jsonString[start..<end]

      let cmd = "executePopupKey(\"\(id)\",\"\(escapedText)\");"
      webView!.evaluateJavaScript(cmd, completionHandler: nil)
    } catch {
      let message = "\(String(describing: error))"
      os_log("%{public}s", log:KeymanEngineLogger.ui, type: .error, message)
      SentryManager.capture(error, message: message)
      return
    }
  }

  func setOskWidth(_ width: Int) {
    webView?.evaluateJavaScript("setOskWidth(\(width));", completionHandler: nil)
  }

  func setOskHeight(_ height: Int) {
    webView?.evaluateJavaScript("setOskHeight(\(height));", completionHandler: nil)
  }

  func setPopupVisible(_ visible: Bool) {
    webView!.evaluateJavaScript("popupVisible(\(visible));", completionHandler: nil)
  }

  private func setSpacebarText(_ mode: SpacebarText) {
    webView!.evaluateJavaScript("setSpacebarText('\(mode.rawValue)');", completionHandler: nil);
  }

  func updateSpacebarText() {
    let userData = Storage.active.userDefaults
    setSpacebarText(userData.optSpacebarText)
  }

  func setCursorRange(_ range: NSRange) {
    if range.location != NSNotFound {
      webView!.evaluateJavaScript("setCursorRange(\(range.location),\(range.length));", completionHandler: nil)
      self.currentCursorRange = range
    }
  }

  func setText(_ text: String?) {
    var text = text ?? ""

    // Remove any system-added LTR/RTL marks.
    text = text.replacingOccurrences(of: "\u{200e}", with: "") // Unicode's LTR codepoint
    text = text.replacingOccurrences(of: "\u{200f}", with: "") // Unicode's RTL codepoint (v1)
    text = text.replacingOccurrences(of: "\u{202e}", with: "") // Unicode's RTL codepoint (v2)

    do {
      let encodingArray = [ text ];
      let jsonString = try String(data: JSONSerialization.data(withJSONObject: encodingArray), encoding: .utf8)!
      let start = jsonString.index(jsonString.startIndex, offsetBy: 2)
      let end = jsonString.index(jsonString.endIndex, offsetBy: -2)
      let jsonText = jsonString[start..<end]

      self.currentText = String(jsonText)
      webView!.evaluateJavaScript("setKeymanVal(\"\(jsonText)\");", completionHandler: nil)
    } catch {
      os_log("%{public}s", log: KeymanEngineLogger.engine, type: .error, error.localizedDescription)
      SentryManager.capture(error.localizedDescription)
    }
  }

  func resetContext() {
    webView!.evaluateJavaScript("doResetContext();", completionHandler: nil)
  }

  private func fontObject(from font: Font?, keyboard: InstallableKeyboard, isOsk: Bool) -> [String: Any]? {
    guard let font = font else {
      return nil
    }
    // family does not have to match the name in the font file. It only has to be unique.
    return [
      "family": "\(keyboard.id)__\(isOsk ? "osk" : "display")",
      "files": font.source.map { storage.fontURL(forResource: keyboard, filename: $0)!.absoluteString }
    ]
  }

  func setKeyboard(_ keyboard: InstallableKeyboard) throws  {
    let fileURL = storage.keyboardURL(for: keyboard)
    var stub: [String: Any] = [
      "KI": "Keyboard_\(keyboard.id)",
      "KN": keyboard.name,
      "KLC": keyboard.languageID,
      "KL": keyboard.languageName,
      "KF": fileURL.absoluteString
    ]

    if let packageID = keyboard.packageID {
      stub["KP"] = packageID
    }

    if let displayName = keyboard.displayName {
      stub["displayName"] = displayName
    }

    // Warning:  without special handling, any `guard` that fails here can trigger an
    // infinite keyboard reload, as the keyboard page itself will note that the keyboard
    // failed to initialize properly.
    guard FileManager.default.fileExists(atPath: fileURL.path) else {
      let event = Sentry.Event(level: .error)
      let errorMessage = "File missing for keyboard"
      event.message = SentryMessage(formatted: errorMessage)
      event.extra = [ "id": keyboard.id, "file": fileURL ]
      if let packageID = keyboard.packageID {
        event.extra?["package"] = packageID
      }
      SentryManager.capture(event)
      os_log("%{public}s id: %s file: %{public}s", log: KeymanEngineLogger.resources, type: .error, errorMessage, keyboard.id, fileURL as CVarArg)
      throw KeyboardError.fileMissing
    }

    let displayFont = fontObject(from: keyboard.font, keyboard: keyboard, isOsk: false)
    let oskFont = fontObject(from: keyboard.oskFont, keyboard: keyboard, isOsk: true) ?? displayFont
    if let displayFont = displayFont {
      stub["KFont"] = displayFont
    }
    if let oskFont = oskFont {
      stub["KOskFont"] = oskFont
    }

    let data: Data
    do {
      data = try JSONSerialization.data(withJSONObject: stub, options: [])
    } catch {
      let event = Sentry.Event(error: error)
      let errorMessage = "Failed to serialize keyboard stub:"
      event.message = SentryMessage(formatted: "Failed to serialize keyboard stub: \(error)")
      event.extra = [:]
      event.extra!["id"] = stub["KI"]
      event.extra!["package"] = stub["KP"]

      SentryManager.capture(event)
      os_log("%{public}s id: %{public}s file: %{public}s", log: KeymanEngineLogger.resources, type: .error, errorMessage, keyboard.id, fileURL as CVarArg)
      throw KeyboardError.keyboardLoadingError
    }
    guard let stubString = String(data: data, encoding: .utf8) else {
      let event = Sentry.Event(level: .error)
      event.message = SentryMessage(formatted: "Failed to create keyboard stub string for embedded KMW")
      event.extra = [:]
      event.extra!["id"] = stub["KI"]
      event.extra!["package"] = stub["KP"]

      os_log("Failed to create keyboard stub string for embedded KMW", log: KeymanEngineLogger.ui, type: .error)
      SentryManager.capture(event)
      throw KeyboardError.keyboardLoadingError
    }
    let message = "Keyboard stub built for \(keyboard.id)"
    os_log("%{public}s", log: KeymanEngineLogger.resources, type: .info, message)
    SentryManager.breadcrumb(message)
    webView!.evaluateJavaScript("setKeymanLanguage(\(stubString));", completionHandler: nil)
  }

  func deregisterLexicalModel(_ lexicalModel: InstallableLexicalModel) {
    webView!.evaluateJavaScript("keyman.removeModel(\"\(lexicalModel.id)\")")
  }

  func registerLexicalModel(_ lexicalModel: InstallableLexicalModel) throws {
    let fileURL = storage.lexicalModelURL(for: lexicalModel)
    let stub: [String: Any] = [
      "id": lexicalModel.id,
      "languages": [lexicalModel.languageID], // Change when InstallableLexicalModel is updated to store an array
      "path": fileURL.absoluteString
    ]

    guard FileManager.default.fileExists(atPath: fileURL.path) else {
      let event = Sentry.Event(level: .error)
      let errorMessage = "File missing for lexical model"
      event.message = SentryMessage(formatted: errorMessage)
      event.extra = [ "id": lexicalModel.id, "file": fileURL ]
      if let packageID = lexicalModel.packageID {
        event.extra?["package"] = packageID
      }
      
      os_log("%{public}s: %{public}s", log: KeymanEngineLogger.resources, type: .error, errorMessage, lexicalModel.id)
      SentryManager.capture(event)
      throw KeyboardError.fileMissing
    }

    let data: Data
    do {
      data = try JSONSerialization.data(withJSONObject: stub, options: [])
    } catch {
      let event = Sentry.Event(error: error)
      let errorMessage = "Failed to serialize lexical model stub:"
      event.message = SentryMessage(formatted: "Failed to serialize lexical model stub: \(error)")
      event.extra = [:]
      event.extra!["id"] = stub["id"]

      os_log("%{public}s: %{public}s", log: KeymanEngineLogger.resources, type: .error, errorMessage, error.localizedDescription)
      SentryManager.capture(event)
      throw KeyboardError.lexicalModelLoadingError
    }
    guard let stubString = String(data: data, encoding: .utf8) else {
      let event = Sentry.Event(level: .error)
      let errorMessage = "Failed to create lexical model stub string for embedded KMW"
      event.message = SentryMessage(formatted: errorMessage)
      event.extra = [:]
      event.extra!["id"] = stub["id"]

      os_log("%{public}s", log: KeymanEngineLogger.resources, type: .error, errorMessage)
      SentryManager.capture(event)
      throw KeyboardError.lexicalModelLoadingError
    }

    let message = "LexicalModel stub built for \(lexicalModel.id)"
    os_log("%{public}s", log: KeymanEngineLogger.resources, type: .info, message)
    SentryManager.breadcrumb(message)

    if lexicalModel.languageID == Manager.shared.currentKeyboardID?.languageID {
      // We're registering a lexical model for the now-current keyboard.
      // Enact any appropriate language-modeling settings!

      let userDefaults = Storage.active.userDefaults

      let predict = userDefaults.predictSettingForLanguage(languageID: lexicalModel.languageID)
      let correct = userDefaults.correctSettingForLanguage(languageID: lexicalModel.languageID)

      // Pass these off to KMW!
      // We do these first so that they're automatically set for the to-be-registered model in advance.
      webView!.evaluateJavaScript("enableSuggestions(\(stubString), \(predict), \(correct))")
      self.activeModel = predict
    } else {  // We're registering a model in the background - don't change settings.
      webView!.evaluateJavaScript("keyman.addModel(\(stubString));", completionHandler: nil)
    }

    setBannerHeight(to: Int(InputViewController.topBarHeight))
  }

  func showBanner(_ display: Bool) {
    let message = "Changing banner's alwaysShow property to \(display)"
    os_log("%{public}s", log: KeymanEngineLogger.settings, type: .debug, message)
    SentryManager.breadcrumb(message, category: "engine", sentryLevel: .debug)
    webView?.evaluateJavaScript("showBanner(\(display ? "true" : "false"))", completionHandler: nil)
  }

  func setBannerImage(to path: String) {
    bannerImgPath = path // Save the path in case delayed initializaiton is needed.
    var logString: String
    if path.contains("base64") || path.count > 256 {
      logString = "<base64 image>"
    } else {
      logString = path
    }
    let message = "Banner image path: '\(logString).'"
    os_log("%{public}s", log: KeymanEngineLogger.ui, type: .debug, message)
    webView?.evaluateJavaScript("setBannerImage(\"\(path)\");", completionHandler: nil)
  }

  func setBannerHeight(to height: Int) {
    webView?.evaluateJavaScript("setBannerHeight(\(height));", completionHandler: nil)
  }

  /**
   * Ensures that the embedded KMW instance uses the app's current error-report toggle setting.
   * This is always called during keyboard page initialization and may also be called any time
   * thereafter for settings updates.
   */
  func setSentryState(enabled: Bool = SentryManager.enabled) {
    // This may be called before the page - and thus the `sentryManager` variable -
    // is ready.  It's a known limitation, so why log error reports for it?
    webView?.evaluateJavaScript("try { sentryManager.enabled = \(enabled ? "true" : "false") } catch(err) { }")
  }
}

// MARK: - WKScriptMessageHandler
extension KeymanWebViewController: WKScriptMessageHandler {
  func userContentController(_ userContentController: WKUserContentController,
                             didReceive message: WKScriptMessage) {
    guard let fragment = message.body as? String, !fragment.isEmpty else {
      return
    }

    if fragment.hasPrefix("#insertText-") {
      let dnRange = fragment.range(of: "+dn=")!
      let sRange = fragment.range(of: "+s=")!
      let drRange = fragment.range(of: "+dr=")!

      let dn = Int(fragment[dnRange.upperBound..<sRange.lowerBound])!
      let s = fragment[sRange.upperBound..<drRange.lowerBound]
      // This computes the number of requested right-deletion characters.
      // Use it when we're ready to implement that.
      // Our .insertText will need to be adjusted accordingly.
      _ = Int(fragment[drRange.upperBound...])!

      // KMW uses dn == -1 to perform special processing of deadkeys.
      // This is handled outside of Swift so we don't delete any characters.
      let numCharsToDelete = max(0, dn)
      let newText = String(s).stringFromUTF16CodeUnits() ?? ""
      insertText(self, numCharsToDelete: numCharsToDelete, newText: newText)
      delegate?.insertText(self, numCharsToDelete: numCharsToDelete, newText: newText)
    } else if fragment.hasPrefix("#showKeyPreview-") {
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

      let frame = KeymanWebViewController.keyFrame(x: x, y: y, w: w, h: h)
      let preview = t.stringFromUTF16CodeUnits() ?? ""
      showKeyPreview(self, keyFrame: frame, preview: preview)
      delegate?.showKeyPreview(self, keyFrame: frame, preview: preview)
    } else if fragment.hasPrefix("#dismissKeyPreview-") {
      dismissKeyPreview(self)
      delegate?.dismissKeyPreview(self)
    } else if fragment.hasPrefix("#showMore-") {
      let baseFrameKey = fragment.range(of: "+baseFrame=")!
      let keysKey = fragment.range(of: "+keys=")!
      let fontKey = fragment.range(of: "+font=")
      let baseFrame = fragment[baseFrameKey.upperBound..<keysKey.lowerBound]
      let keys = fragment[keysKey.upperBound..<(fontKey?.lowerBound ?? fragment.endIndex)]
      let useSpecialFont = fontKey != nil

      let frameComponents = baseFrame.components(separatedBy: ",")
      let x = CGFloat(Float(frameComponents[0])!)
      let y = CGFloat(Float(frameComponents[1])!)
      let w = CGFloat(Float(frameComponents[2])!)
      let h = CGFloat(Float(frameComponents[3])!)
      let frame = KeymanWebViewController.keyFrame(x: x, y: y, w: w, h: h)

      let keyArray = keys.components(separatedBy: ";")
      var subkeyIDs: [String] = []
      var subkeyTexts: [String] = []

      for key in keyArray {
        let values = key.components(separatedBy: ":")
        switch values.count {
        case 1:
          let id = values[0]
          subkeyIDs.append(id)
          // id is in the form layer-keyID. We only process keyIDs with prefix U_.
          if let index = id.range(of: "-U_", options: .backwards)?.upperBound,
            let codepoint = UInt32(id[index...], radix: 16),
            let scalar = Unicode.Scalar(codepoint) {
            subkeyTexts.append(String(Character(scalar)))
          } else {
            subkeyTexts.append("")
          }
        case 2:
          subkeyIDs.append(values[0])
          subkeyTexts.append(values[1].stringFromUTF16CodeUnits() ?? "")
        default:
          let message = "Unexpected subkey key: \(key)"
          os_log("%{public}s", log: KeymanEngineLogger.ui, type: .error, message)
        }
      }

      showSubkeys(self,
                  keyFrame: frame,
                  subkeyIDs: subkeyIDs,
                  subkeyTexts: subkeyTexts,
                  useSpecialFont: useSpecialFont)
      delegate?.showSubkeys(self,
                            keyFrame: frame,
                            subkeyIDs: subkeyIDs,
                            subkeyTexts: subkeyTexts,
                            useSpecialFont: useSpecialFont)

      self.touchHoldBegan()
    } else if fragment.hasPrefix("#menuKeyDown-") {
      perform(#selector(self.menuKeyHeld), with: self, afterDelay: 0.5)
      menuKeyDown(self)
      delegate?.menuKeyDown(self)
    } else if fragment.hasPrefix("#menuKeyUp-") {
      // Blocks summoning the globe-key menu for quick taps.  (Hence the 0.5 delay.)
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: #selector(self.menuKeyHeld), object: self)
      menuKeyUp(self)
      delegate?.menuKeyUp(self)
    } else if fragment.hasPrefix("#hideKeyboard-") {
      hideKeyboard(self)
      Manager.shared.hideKeyboard()
    } else if fragment.hasPrefix("ios-log:#iOS#") {
      let message = fragment.dropFirst(13)
      // This may need filtering for proper use with Sentry?
      // Then again, if KMW is logging it... we already have to worry
      // about it showing up in Web-oriented Sentry logs.
      
      let logMessage = "KMW Log: \(message)"
      os_log("%{public}s", log: KeymanEngineLogger.engine, type: .info, logMessage)
      SentryManager.breadcrumb(logMessage)
    } else if fragment.hasPrefix("#beep-") {
      beep(self)
      delegate?.beep(self)
    } else if fragment.hasPrefix("#suggestPopup"){
      let cmdKey = fragment.range(of: "+cmd=")!
      let cmdStr = fragment[cmdKey.upperBound..<fragment.endIndex]

      let cmdData = cmdStr.data(using: .utf16)
      let decoder = JSONDecoder()

      do {
        let cmd = try decoder.decode(SuggestionPopup.self, from: cmdData!)
        let message = "Longpress detected on suggestion: \"\(cmd.suggestion.displayAs)\"."
        os_log("%{public}s", log: KeymanEngineLogger.ui, type: .debug, message)
     } catch {
       let message = "Unexpected JSON parse error: \(error)."
       os_log("%{public}s", log:KeymanEngineLogger.engine, type: .error, message)
       SentryManager.capture(error, message: message)
      }

      // Will need processing upon extraction from the resulting object.
//      let frameComponents = baseFrame.components(separatedBy: ",")
//      let x = CGFloat(Float(frameComponents[0])!)
//      let y = CGFloat(Float(frameComponents[1])!)
//      let w = CGFloat(Float(frameComponents[2])!)
//      let h = CGFloat(Float(frameComponents[3])!)
//      let frame = KeymanWebViewController.keyFrame(x: x, y: y, w: w, h: h)

    } else {
      let message = "Unexpected KMW event: \(fragment)"
      os_log("%{public}s", log: KeymanEngineLogger.engine, type: .info, message)
      SentryManager.capture(message)
    }
  }

  private static func keyFrame(x: CGFloat, y: CGFloat, w: CGFloat, h: CGFloat) -> CGRect {
    return CGRect(x: x - (w/2), y: y, width: w, height: h)
  }

  public func beep(_ keymanWeb: KeymanWebViewController) {
    let vibrationSupport = Manager.shared.vibrationSupportLevel
    let kSystemSoundID_MediumVibrate: SystemSoundID = 1520

    if vibrationSupport == .none {
      // TODO:  Find something we can do visually and/or audibly to provide feedback.
    } else if vibrationSupport == .basic {
      // The publicly-suggested kSystemSoundID_Vibrate lasts for 0.4 seconds.
      // Better than nothing, though it's easily too long for a proper beep.
      AudioServicesPlaySystemSound(kSystemSoundID_Vibrate)
    } else if vibrationSupport == .basic_plus {
      // Code 1519 is near-undocumented, but should result in a 'weaker'/shorter vibration.
      // Corresponds directly to UIImpactFeedbackGenerator below, but on select phones that
      // don't support that part of the API.
      //
      // Ref: https://stackoverflow.com/questions/10570553/how-to-set-iphone-vibrate-length/44495798#44495798
      // Not usable by older iPhone models.
      AudioServicesPlaySystemSound(kSystemSoundID_MediumVibrate)
    } else { // if vibrationSupport == .taptic
      // Available with iPhone 7 and beyond, we can now produce nicely customized haptic feedback.
      // We use this style b/c it's short, and in essence it is a minor UI element collision -
      // a single key with blocked (erroneous) output.
      // Oddly, is a closer match to SystemSoundID 1520 than 1521.
      let vibrator = UIImpactFeedbackGenerator(style: UIImpactFeedbackGenerator.FeedbackStyle.heavy)
      vibrator.impactOccurred()
    }
  }
}

// MARK: - WKNavigationDelegate
extension KeymanWebViewController: WKNavigationDelegate {
  func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
    guard let url = webView.url else {
      return
    }
    guard url.lastPathComponent == Resources.kmwFilename && (url.fragment?.isEmpty ?? true) else {
      return
    }
    keyboardLoaded(self)
    delegate?.keyboardLoaded(self)
  }
}

// MARK: - KeymanWebDelegate
extension KeymanWebViewController: KeymanWebDelegate {
  func keyboardLoaded(_ keymanWeb: KeymanWebViewController) {
    delegate?.keyboardLoaded(keymanWeb)

    isLoading = false
    let message = "Loaded keyboard."
    os_log("%{public}s", log: KeymanEngineLogger.engine, type: .info, message)
    SentryManager.breadcrumb(message, sentryLevel: .debug)

    self.setSentryState()
    resizeKeyboard()

    // There may have been attempts to set these values before the keyboard loaded!
    self.setText(self.currentText)
    if let cursorRange = self.currentCursorRange {
      self.setCursorRange(cursorRange)
    }

    var newKb = Manager.shared.currentKeyboard

    if !shouldReload { // otherwise, we automatically reload anyway.
      let userData = Manager.shared.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
      if newKb == nil {
        if let id = userData.currentKeyboardID {
          if let kb = Storage.active.userDefaults.userKeyboard(withFullID: id) {
            newKb = kb
          }
        }

        // Failsafe in case the previous lookup fails - at least load A keyboard.
        if newKb == nil, let userKbs = Storage.active.userDefaults.userKeyboards, !userKbs.isEmpty {
          newKb = userKbs[0]
        }
      }
      let message = "Setting initial keyboard."
      os_log("%{public}s", log: KeymanEngineLogger.engine, type: .info, message)
      SentryManager.breadcrumb(message)

      // Compare against resetKeyboard & Manager.setKeyboard;
      // setting this to `nil` allows us to force keyboard reloads when needed.
      Manager.shared.currentKeyboardID = nil

      if !Manager.shared.setKeyboard(newKb!) {
        // The keyboard couldn't load for... whatever reason.
        // Default-keyboard fallback time.  We _know_ that one should work.
        do {
          var defaultWasMissing = false
          // Ensure the default keyboard is installed in this case.
          if !(Storage.active.userDefaults.userKeyboards?.contains(where: {$0.fullID == Defaults.keyboardID }) ?? true) ||
            !FileManager.default.fileExists(atPath: Storage.active.keyboardURL(for: Defaults.keyboard).path) {
            defaultWasMissing = true
            try Storage.active.installDefaultKeyboard(from: Resources.bundle)
          }

          // Ensures we don't infinitely try to reload the keyboard.
          if(defaultWasMissing || newKb!.fullID != Defaults.keyboard.fullID) {
            // Be sure to force a reset again.
            Manager.shared.currentKeyboardID = nil
            _ = Manager.shared.setKeyboard(Defaults.keyboard)
          }
        } catch {
          let message = "Could not load default keyboard as a fallback for keyboard loading failure"
          os_log("%{public}s", log: KeymanEngineLogger.engine, type: .error, message)
          SentryManager.capture(message, sentryLevel: .fatal)
        }
        newKb = Defaults.keyboard
      }
    }

    // in case `shouldReload == true`.  Is set otherwise above.
    if(newKb == nil) {
      newKb = Defaults.keyboard
    }

    updateSpacebarText()
    updateShowBannerSetting()
    setBannerImage(to: bannerImgPath)
    // Reset the keyboard's size.
    keyboardSize = kbSize

    fixLayout()

    // Will trigger Manager's `keyboardLoaded` method.
    NotificationCenter.default.post(name: Notifications.keyboardLoaded, object: self, value: newKb!)

    // "Should reload" the keyboard assets.  (Not the host page.)
    if shouldReload {
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: #selector(self.resetKeyboard), object: nil)
      perform(#selector(self.resetKeyboard), with: nil, afterDelay: 0.25)
      shouldReload = false
    }
  }

  func updateShowBannerSetting() {
    let userData = Storage.active.userDefaults
    let alwaysShow = userData.bool(forKey: Key.optShouldShowBanner)
    if !Manager.shared.isSystemKeyboard {
      showBanner(false)
    } else {
      showBanner(alwaysShow)
    }
  }

  func insertText(_ view: KeymanWebViewController, numCharsToDelete: Int, newText: String) {
    dismissHelpBubble()
    Manager.shared.isKeymanHelpOn = false
  }

  func showKeyPreview(_ view: KeymanWebViewController, keyFrame: CGRect, preview: String) {
    if UIDevice.current.userInterfaceIdiom == .pad || isSubKeysMenuVisible {
      return
    }

    dismissKeyPreview()
    clearSubKeyArrays()

    keyPreviewView = KeyPreviewView(frame: keyFrame)

    keyPreviewView!.setLabelText(preview)

    let oskFontName = Manager.shared.oskFontNameForKeyboard(withFullID: Manager.shared.currentKeyboardID!)
      ?? Manager.shared.fontNameForKeyboard(withFullID: Manager.shared.currentKeyboardID!)
    keyPreviewView!.setLabelFont(oskFontName)
    self.view.addSubview(keyPreviewView!)
  }

  func dismissKeyPreview(_ view: KeymanWebViewController) {
    if UIDevice.current.userInterfaceIdiom == .pad || keyPreviewView == nil {
      return
    }

    let dismissKeyPreview = #selector(self.dismissKeyPreview as () -> Void)
    NSObject.cancelPreviousPerformRequests(withTarget: self, selector: dismissKeyPreview, object: nil)
    perform(dismissKeyPreview, with: nil, afterDelay: 0.1)
    clearSubKeyArrays()
  }

  func showSubkeys(_ view: KeymanWebViewController,
                   keyFrame: CGRect,
                   subkeyIDs: [String],
                   subkeyTexts: [String],
                   useSpecialFont: Bool) {
    dismissHelpBubble()
    Manager.shared.isKeymanHelpOn = false
    dismissSubKeys()
    dismissKeyboardMenu()

    subKeyAnchor = keyFrame
    subKeyIDs = subkeyIDs
    subKeyTexts = subkeyTexts
    self.useSpecialFont = useSpecialFont
  }

  func menuKeyUp(_ view: KeymanWebViewController) {
    dismissHelpBubble()
    Manager.shared.isKeymanHelpOn = false
    if Util.isSystemKeyboard {
      let userData = UserDefaults.standard
      userData.set(true, forKey: Key.keyboardPickerDisplayed)
      userData.synchronize()
    }
  }

  func hideKeyboard(_ view: KeymanWebViewController) {
    dismissHelpBubble()
    dismissSubKeys()
    dismissKeyboardMenu()
  }
}

// MARK: - UIGestureRecognizerDelegate
extension KeymanWebViewController: UIGestureRecognizerDelegate {
  public func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer,
                                shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer) -> Bool {
    return true
  }

  // An adaptation of holdAction, just for direct taps.
  @objc func tapAction(_ sender: UITapGestureRecognizer) {
    switch sender.state {
    case .ended:
      // Touch Ended
      guard let subKeysView = subKeysView else {
        return
      }

      let touchPoint = sender.location(in: subKeysView.containerView)
      var buttonClicked = false
      for button in subKeys {
        if button.frame.contains(touchPoint) {
          button.isEnabled = true
          button.isHighlighted = true
          button.backgroundColor = subKeyColorHighlighted
          button.sendActions(for: .touchUpInside)

          buttonClicked = true
        } else {
          button.isHighlighted = false
          button.isEnabled = false
          button.backgroundColor = subKeyColor
        }
      }

      if !buttonClicked {
        clearSubKeyArrays()
      }
    default:
      return
    }
  }

  @objc func holdAction(_ sender: UILongPressGestureRecognizer) {
    switch sender.state {
    case .ended:
      // Touch Ended
      if let subKeysView = subKeysView {
        subKeysView.removeFromSuperview()
        subKeysView.subviews.forEach { $0.removeFromSuperview() }
        self.subKeysView = nil
        setPopupVisible(false)
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
//      // Touch & Hold Began
//      let touchPoint = sender.location(in: sender.view)
//      // Check if touch was for language menu button
//      languageMenuPosition { keyFrame in
//        if keyFrame.contains(touchPoint) {
//          self.delegate?.menuKeyHeld(self)
//          return
//        }
//        self.touchHoldBegan()
//      }
      return
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
    // Is also called for banner longpresses.  Will need a way to properly differentiate.
    let isPad = UIDevice.current.userInterfaceIdiom == .pad
    let fontSize = isPad ? UIFont.buttonFontSize * 2 : UIFont.buttonFontSize

    let oskFontName = Manager.shared.oskFontNameForKeyboard(withFullID: Manager.shared.currentKeyboardID!)
      ?? Manager.shared.fontNameForKeyboard(withFullID: Manager.shared.currentKeyboardID!)

    if subKeyIDs.isEmpty {
      subKeys = []
      return
    }

    subKeys = subKeyTexts.enumerated().map { i, subKeyText in
      let button = UIButton(type: .custom)
      button.tag = i
      button.backgroundColor = subKeyColor
      button.setRoundedBorder(withRadius: 4.0, borderWidth: 1.0, color: .gray)
      button.setTitleColor(Colors.keyText, for: .disabled)
      button.setTitleColor(Colors.keyText, for: .highlighted)
      button.setTitleColor(Colors.keyText, for: .normal)

      if let oskFontName = oskFontName {
        button.titleLabel?.font = UIFont(name: oskFontName, size: fontSize)
      } else {
        button.titleLabel?.font = UIFont.systemFont(ofSize: fontSize)
      }

      if useSpecialFont{
        if FontManager.shared.registerFont(at: Storage.active.specialOSKFontURL),
          let fontName = FontManager.shared.fontName(at: Storage.active.specialOSKFontURL) {
            button.titleLabel?.font = UIFont(name: fontName, size: fontSize)
        }
        button.setTitleColor(.gray, for: .disabled)
      }

      button.addTarget(self, action: #selector(subKeyButtonClick), for: .touchUpInside)

      // Detect the text width for subkeys.  The 'as Any' silences an inappropriate warning from Swift.
      let textSize = subKeyText.size(withAttributes: [NSAttributedString.Key.font: button.titleLabel?.font! as Any])
      var displayText = subKeyText

      if textSize.width <= 0 && subKeyText.count > 0 {
        // It's probably a diacritic in need of a combining character!
        // Also check if the language is RTL!
        if Manager.shared.currentKeyboard?.isRTL ?? false {
          displayText = "\u{200f}\u{25cc}" + subKeyText
        } else {
          displayText = "\u{25cc}" + subKeyText
        }
      }

      button.setTitle(displayText, for: .normal)
      button.tintColor = Colors.popupKeyTint
      button.isEnabled = false
      return button
    }

    dismissKeyPreview()
    subKeysView = SubKeysView(keyFrame: subKeyAnchor, subKeys: subKeys)
    view.addSubview(subKeysView!)

    let tap = UITapGestureRecognizer(target: self, action: #selector(self.tapAction))
    subKeysView!.addGestureRecognizer(tap)

    let hold = UILongPressGestureRecognizer(target: self, action: #selector(self.holdAction))
    hold.minimumPressDuration = 0.01
    hold.delegate = self
    subKeysView!.addGestureRecognizer(hold)
    setPopupVisible(true)
  }

  @objc func menuKeyHeld(_ keymanWeb: KeymanWebViewController) {
    self.delegate?.menuKeyHeld(self)
  }

  @objc func subKeyButtonClick(_ sender: UIButton) {
    let keyIndex = sender.tag
    if keyIndex < subKeyIDs.count && keyIndex < subKeyTexts.count {
      let subKeyID = subKeyIDs[keyIndex]
      let subKeyText = subKeyTexts[keyIndex]
      executePopupKey(id: subKeyID, text: subKeyText)
    }
    subKeys.removeAll()
    subKeyIDs.removeAll()
    subKeyTexts.removeAll()
  }
}

// MARK: - Manage views
extension KeymanWebViewController {
  // MARK: - Sizing
  public var keyboardHeight: CGFloat {
    return keyboardSize.height
  }

  func constraintTargetHeight(isPortrait: Bool) -> CGFloat {
    return KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: isPortrait)?.keyboardHeight ?? 216 // default for ancient devices
  }

  var keyboardWidth: CGFloat {
    return keyboardSize.width
  }

  func initKeyboardSize() {
    var width: CGFloat
    var height: CGFloat
    width = UIScreen.main.bounds.width

    if Util.isSystemKeyboard {
      height = constraintTargetHeight(isPortrait: InputViewController.isPortrait)
    } else {
      height = constraintTargetHeight(isPortrait: UIDevice.current.orientation.isPortrait)
    }

    keyboardSize = CGSize(width: width, height: height)
  }

  var keyboardSize: CGSize {
    get {
      if kbSize.equalTo(CGSize.zero) {
        initKeyboardSize()
      }

      return kbSize
    }
    set(size) {
      // Only perform set management code if the size values has actually changed.
      // We tend to get a lot of noise on this, so filtering like this also helps increase
      // stability and performance.
      //
      // Note that since viewWillLayoutSubviews is triggered by touch events (for some reason) as
      // well as view transitions, this helps to prevent issues that arise from replacing the OSK
      // on touch events that would otherwise occur - at present, a resize operation in KMW
      // automatically replaces the OSK.
      if kbSize != size {
        kbSize = size
        setOskWidth(Int(size.width))
        setOskHeight(Int(size.height))
      }
    }
  }

  @objc func resizeDelay() {
    // + 1000 to work around iOS bug with resizing on landscape orientation. Technically we only
    // need this for landscape but it doesn't hurt to do it with both. 1000 is a big number that
    // should hopefully work on all devices.
    let kbWidth = keyboardWidth
    let kbHeight = keyboardHeight
    view.frame = CGRect(x: 0.0, y: 0.0, width: kbWidth, height: kbHeight + 1000)
  }

  // Keyman interaction
  func resizeKeyboard() {
    fixLayout()

     // Ensures the width and height are properly updated.
    // Note:  System Keyboard init currently requires this for the keyboard to display properly
    // the first time.
    setOskWidth(Int(kbSize.width))
    setOskHeight(Int(kbSize.height))
  }

  func resetKeyboardState() {
    dismissSubKeys()
    dismissKeyPreview()
    dismissKeyboardMenu()
    resizeKeyboard()
  }

  // Used for a selector; keep @objc!
  @objc func resetKeyboard() {
    let keyboard = Manager.shared.currentKeyboard
    Manager.shared.currentKeyboardID = nil

    if let keyboard = keyboard {
      let message = "Current keyboard is set."
      os_log("%{public}s", log: KeymanEngineLogger.engine, type: .info, message)
      SentryManager.breadcrumb(message)
      _ = Manager.shared.setKeyboard(keyboard)
    } else if let keyboard = Storage.active.userDefaults.userKeyboards?[safe: 0] {
      let message = "Using user's default keyboard."
      os_log("%{public}s", log: KeymanEngineLogger.engine, type: .info, message)
      SentryManager.breadcrumb(message)
      _ = Manager.shared.setKeyboard(keyboard)
    } else {
      let message = "Using app-default keyboard."
      os_log("%{public}s", log: KeymanEngineLogger.engine, type: .info, message)
      SentryManager.breadcrumb(message)
      _ = Manager.shared.setKeyboard(Defaults.keyboard)
    }
  }

  // MARK: - Show/hide views
  func reloadKeyboard() {
    webView!.loadFileURL(Storage.active.kmwURL, allowingReadAccessTo: Storage.active.baseDir)
    isLoading = true

    updateSpacebarText()
    // Check for a change of "always show banner" state
    updateShowBannerSetting()
  }

  /*
   * Implemented as a workaround for a weird bug (likely within iOS) in which the
   * InputViewController and KeymanWebViewController constructors (and thus, `loadView`)
   * are sometimes completely bypassed during system-keyboard use.
   * See https://github.com/keymanapp/keyman/issues/3985
   */
  func verifyLoaded() {
    // Test: are we currently loading?  If so, don't worry 'bout a thing.
    if !isLoading {
      webView!.evaluateJavaScript("verifyLoaded()", completionHandler: nil)
    }
  }

  @objc func showHelpBubble() {
    // Help bubble is always disabled for system-wide keyboard
    if Util.isSystemKeyboard || keyboardMenuView != nil {
      return
    }

    languageMenuPosition { keyFrame in
      // We should calculate the center point between the origin and the right-hand coordinate of the key.
      // keyFrame.origin seems to use the left-hand (minX) edge, which looks ugly.  Y coord's good, though.
      let tipRootPoint = CGPoint(x: keyFrame.midX, y: keyFrame.origin.y)
      self.showHelpBubble(at: tipRootPoint)
    }
  }

  // TODO: The bulk of this should be moved to PopoverView
  private func showHelpBubble(at point: CGPoint) {
    self.helpBubbleView?.removeFromSuperview()
    let helpBubbleView = PopoverView(frame: CGRect.zero)
    self.helpBubbleView = helpBubbleView
    helpBubbleView.backgroundColor = Colors.helpBubbleGradient1
    helpBubbleView.backgroundColor2 = Colors.helpBubbleGradient2
    helpBubbleView.borderColor = Colors.popupBorder

    let isPad = UIDevice.current.userInterfaceIdiom == .pad
    let sizeMultiplier = CGFloat(isPad ? 1.5 : 1.0)
    let frameWidth = 90.0 * sizeMultiplier
    let frameHeight = (40.0 + helpBubbleView.arrowHeight) * sizeMultiplier
    let fontSize = 10.0 * sizeMultiplier

    let inputViewFrame = view.frame
    let screenWidth = inputViewFrame.size.width

    // TODO: Refactor this out
    let isPortrait = UIDevice.current.orientation.isPortrait

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
    //helpText.textColor = UIColor.darkText
    helpText.lineBreakMode = .byWordWrapping
    helpText.numberOfLines = 0
    helpText.text = keyboardChangeHelpText
    helpBubbleView.addSubview(helpText)
    view.addSubview(helpBubbleView)
  }

  func dismissHelpBubble() {
    helpBubbleView?.removeFromSuperview()
    helpBubbleView = nil
  }

  @objc func dismissKeyPreview() {
    keyPreviewView?.removeFromSuperview()
    keyPreviewView = nil
  }

  var isSubKeysMenuVisible: Bool {
    return subKeysView != nil
  }

  func dismissSubKeys() {
    if let subKeysView = subKeysView {
      subKeysView.removeFromSuperview()
      subKeysView.subviews.forEach { $0.removeFromSuperview() }
      self.subKeysView = nil
      setPopupVisible(false)
    }
    clearSubKeyArrays()
  }

  func clearSubKeyArrays() {
    if subKeysView == nil {
      subKeys.removeAll()
      subKeyIDs.removeAll()
      subKeyTexts.removeAll()
    }
  }

  func showHelpBubble(afterDelay delay: TimeInterval) {
    helpBubbleView?.removeFromSuperview()
    let showHelpBubble = #selector(self.showHelpBubble as () -> Void)
    NSObject.cancelPreviousPerformRequests(withTarget: self, selector: showHelpBubble, object: nil)
    perform(showHelpBubble, with: nil, afterDelay: delay)
  }

  func showKeyboardMenu(_ ic: InputViewController, closeButtonTitle: String?) {
    let parentView = ic.view ?? view
    languageMenuPosition { keyFrame in
      if keyFrame != .zero {
        self.keyboardMenuView?.removeFromSuperview()
        self.keyboardMenuView = KeyboardMenuView(keyFrame: keyFrame, inputViewController: ic,
                                                 closeButtonTitle: closeButtonTitle)
        parentView?.addSubview(self.keyboardMenuView!)
        self.keyboardMenuView!.flashScrollIndicators()
      }
    }
  }

  func dismissKeyboardMenu() {
    keyboardMenuView?.removeFromSuperview()
    keyboardMenuView = nil
  }

  var isKeyboardMenuVisible: Bool {
    return keyboardMenuView != nil
  }
}

// MARK: - Keyboard Notifications
extension KeymanWebViewController {
  @objc func keyboardWillShow(_ notification: Notification) {
    dismissSubKeys()
    dismissKeyPreview()
    resizeKeyboard()

    if Manager.shared.isKeymanHelpOn {
      showHelpBubble(afterDelay: 1.5)
    }
  }

  @objc func keyboardWillHide(_ notification: Notification) {
    dismissHelpBubble()
    dismissSubKeys()
    dismissKeyPreview()
  }
}
