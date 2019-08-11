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

private let keyboardChangeHelpText = "Tap here to change keyboard"

private let subKeyColor = #colorLiteral(red: 244.0 / 255.0, green: 244.0 / 255.0, blue: 244.0 / 255.0, alpha: 1.0)
private let subKeyColorHighlighted = #colorLiteral(red: 136.0 / 255.0, green: 136.0 / 255.0, blue: 1.0, alpha: 1.0)

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

// MARK: - UIViewController
class KeymanWebViewController: UIViewController {
  let storage: Storage
  weak var delegate: KeymanWebDelegate?
  private var useSpecialFont = false

  // Views
  var webView: WKWebView?
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

    let userContentController = WKUserContentController()
    userContentController.add(self, name: "keyman")
    config.userContentController = userContentController

    webView = WKWebView(frame: CGRect(origin: .zero, size: keyboardSize), configuration: config)
    webView!.isOpaque = false
    webView!.translatesAutoresizingMaskIntoConstraints = false
    webView!.backgroundColor = UIColor.clear
    webView!.navigationDelegate = self
    webView!.scrollView.isScrollEnabled = false
    
    view = webView

    // Set UILongPressGestureRecognizer to show sub keys
    let hold = UILongPressGestureRecognizer(target: self, action: #selector(self.holdAction))
    hold.minimumPressDuration = 0.5
    hold.delegate = self
    view.addGestureRecognizer(hold)

    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillShow),
                                           name: .UIKeyboardWillShow, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillHide),
                                           name: .UIKeyboardWillHide, object: nil)

    reloadKeyboard()
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
  }
  
  // Very useful for immediately adjusting the WebView's properties upon loading.
  override func viewDidAppear(_ animated: Bool) {
    fixLayout()
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
      log.error(error)
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

  func setCursorRange(_ range: NSRange) {
    if range.location != NSNotFound {
      webView!.evaluateJavaScript("setCursorRange(\(range.location),\(range.length));", completionHandler: nil)
    }
  }

  func setText(_ text: String?) {
    var text = text ?? ""
    // Remove any system-added LTR/RTL marks.
    text = text.replacingOccurrences(of: "\u{200e}", with: "") // Unicode's LTR codepoint
    text = text.replacingOccurrences(of: "\u{200f}", with: "") // Unicode's RTL codepoint (v1)
    text = text.replacingOccurrences(of: "\u{202e}", with: "") // Unicode's RTL codepoint (v2)

    // JavaScript escape-sequence encodings.
    text = text.replacingOccurrences(of: "\\", with: "\\\\")
    text = text.replacingOccurrences(of: "'", with: "\\'")
    text = text.replacingOccurrences(of: "\n", with: "\\n")
    webView!.evaluateJavaScript("setKeymanVal('\(text)');", completionHandler: nil)
  }
  
  func resetContext() {
    webView!.evaluateJavaScript("keyman.interface.resetContext();", completionHandler: nil)
  }

  func setDeviceType(_ idiom: UIUserInterfaceIdiom) {
    let type: String
    switch idiom {
    case .phone:
      type = "AppleMobile"
    case .pad:
      type = "AppleTablet"
    default:
      log.error("Unexpected interface idiom: \(idiom)")
      return
    }
    webView!.evaluateJavaScript("setDeviceType('\(type)');", completionHandler: nil)
  }

  private func fontObject(from font: Font?, keyboardID: String, isOsk: Bool) -> [String: Any]? {
    guard let font = font else {
      return nil
    }
    // family does not have to match the name in the font file. It only has to be unique.
    return [
      "family": "\(keyboardID)__\(isOsk ? "osk" : "display")",
      "files": font.source.map { storage.fontURL(forKeyboardID: keyboardID, filename: $0).absoluteString }
    ]
  }

  func setKeyboard(_ keyboard: InstallableKeyboard) {
    var stub: [String: Any] = [
      "KI": "Keyboard_\(keyboard.id)",
      "KN": keyboard.name,
      "KLC": keyboard.languageID,
      "KL": keyboard.languageName,
      "KF": storage.keyboardURL(for: keyboard).absoluteString
    ]
    let displayFont = fontObject(from: keyboard.font, keyboardID: keyboard.id, isOsk: false)
    let oskFont = fontObject(from: keyboard.oskFont, keyboardID: keyboard.id, isOsk: true) ?? displayFont
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
      log.error("Failed to serialize keyboard stub: \(error)")
      return
    }
    guard let stubString = String(data: data, encoding: .utf8) else {
      log.error("Failed to create stub string")
      return
    }

    log.debug("Keyboard stub: \(stubString)")
    webView!.evaluateJavaScript("setKeymanLanguage(\(stubString));", completionHandler: nil)
  }
  
  func deregisterLexicalModel(_ lexicalModel: InstallableLexicalModel) {
    webView!.evaluateJavaScript("keyman.modelManager.deregister(\"\(lexicalModel.id)\")")
  }

  func registerLexicalModel(_ lexicalModel: InstallableLexicalModel) {
    let stub: [String: Any] = [
      "id": lexicalModel.id,
      "languages": [lexicalModel.languageID], // Change when InstallableLexicalModel is updated to store an array
      "path": storage.lexicalModelURL(for: lexicalModel).absoluteString
    ]
  
    let data: Data
    do {
      data = try JSONSerialization.data(withJSONObject: stub, options: [])
    } catch {
      log.error("Failed to serialize lexical model stub: \(error)")
      return
    }
    guard let stubString = String(data: data, encoding: .utf8) else {
      log.error("Failed to create stub string")
      return
    }
  
    log.debug("LexicalModel stub: \(stubString)")
    if lexicalModel.languageID == Manager.shared.currentKeyboardID?.languageID {
      // We're registering a lexical model for the now-current keyboard.
      // Enact any appropriate language-modeling settings!
      
      let userDefaults = Storage.active.userDefaults
      
      let predict = userDefaults.predictSettingForLanguage(languageID: lexicalModel.languageID)
      let correct = userDefaults.correctSettingForLanguage(languageID: lexicalModel.languageID)
      
      // Pass these off to KMW!
      // We do these first so that they're automatically set for the to-be-registered model in advance.
      webView!.evaluateJavaScript("enableSuggestions(\(stubString), \(predict), \(correct))")
    } else {  // We're registering a model in the background - don't change settings.
      webView!.evaluateJavaScript("keyman.registerModel(\(stubString));", completionHandler: nil)
    }
    
    setBannerHeight(to: InputViewController.topBarHeight)
  }
  
  func showBanner(_ display: Bool) {
    log.debug("Changing banner's alwaysShow property to \(display).")
    webView?.evaluateJavaScript("showBanner(\(display ? "true" : "false"))", completionHandler: nil)
  }
  
  func setBannerImage(to path: String) {
    bannerImgPath = path // Save the path in case delayed initializaiton is needed.
    log.debug("Banner image path: '\(path).'")
    webView?.evaluateJavaScript("setBannerImage(\"\(path)\");", completionHandler: nil)
  }
  
  func setBannerHeight(to height: Int) {
    // TODO:
    webView?.evaluateJavaScript("setBannerHeight(\(height));", completionHandler: nil)
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
          log.warning("Unexpected subkey key: \(key)")
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
    } else if fragment.hasPrefix("#menuKeyDown-") {
      menuKeyDown(self)
      delegate?.menuKeyDown(self)
    } else if fragment.hasPrefix("#menuKeyUp-") {
      menuKeyUp(self)
      delegate?.menuKeyUp(self)
    } else if fragment.hasPrefix("#hideKeyboard-") {
      hideKeyboard(self)
      Manager.shared.hideKeyboard()
    } else if fragment.hasPrefix("ios-log:#iOS#") {
      let message = fragment.dropFirst(13)
      log.info("KMW Log: \(message)")
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
        log.verbose("Longpress detected on suggestion: \"\(cmd.suggestion.displayAs)\".")
      } catch {
        log.error("Unexpected JSON parse error: \(error).")
      }
      
      // Will need processing upon extraction from the resulting object.
//      let frameComponents = baseFrame.components(separatedBy: ",")
//      let x = CGFloat(Float(frameComponents[0])!)
//      let y = CGFloat(Float(frameComponents[1])!)
//      let w = CGFloat(Float(frameComponents[2])!)
//      let h = CGFloat(Float(frameComponents[3])!)
//      let frame = KeymanWebViewController.keyFrame(x: x, y: y, w: w, h: h)
      
    } else {
      log.error("Unexpected KMW event: \(fragment)")
    }
  }

  private static func keyFrame(x: CGFloat, y: CGFloat, w: CGFloat, h: CGFloat) -> CGRect {
    // kmw adds w/2 to x.
    return CGRect(x: x - w / 2.0, y: y, width: w, height: h)
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
      if #available(iOSApplicationExtension 10.0, *) {
        // Available with iPhone 7 and beyond, we can now produce nicely customized haptic feedback.
        // We use this style b/c it's short, and in essence it is a minor UI element collision -
        // a single key with blocked (erroneous) output.
        // Oddly, is a closer match to SystemSoundID 1520 than 1521.
        let vibrator = UIImpactFeedbackGenerator(style: UIImpactFeedbackGenerator.FeedbackStyle.heavy)
        vibrator.impactOccurred()
      } else {
        // Fallback on earlier feedback style
        AudioServicesPlaySystemSound(kSystemSoundID_MediumVibrate)
      }
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

    log.info("Loaded keyboard.")
    
    resizeKeyboard()
    setDeviceType(UIDevice.current.userInterfaceIdiom)
    
    let shouldReloadKeyboard = Manager.shared.shouldReloadKeyboard
    var newKb = Defaults.keyboard
    if Manager.shared.currentKeyboardID == nil && !shouldReloadKeyboard {
      let userData = Manager.shared.isSystemKeyboard ? UserDefaults.standard : Storage.active.userDefaults
      if let id = userData.currentKeyboardID {
        if let kb = Storage.active.userDefaults.userKeyboard(withFullID: id) {
          newKb = kb
        }
      } else if let userKbs = Storage.active.userDefaults.userKeyboards, !userKbs.isEmpty {
        newKb = userKbs[0]
      }
      log.info("Setting initial keyboard.")
      _ = Manager.shared.setKeyboard(newKb)
    }
    
    if Manager.shared.isSystemKeyboard {
      showBanner(true)
    } else {
      // TODO:  Set banner to visible / not visible based on the toggle in Settings.
      //        Problem:  we need access to the banner image path there.  It's only set for the system keyboard variant!
      showBanner(false)
    }
    setBannerImage(to: bannerImgPath)
    // Reset the keyboard's size.
    keyboardSize = kbSize
    
    fixLayout()

    NotificationCenter.default.post(name: Notifications.keyboardLoaded, object: self, value: newKb)
    if shouldReloadKeyboard {
      NSObject.cancelPreviousPerformRequests(withTarget: self, selector: #selector(self.resetKeyboard), object: nil)
      perform(#selector(self.resetKeyboard), with: nil, afterDelay: 0.25)
      Manager.shared.shouldReloadKeyboard = false
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
      // Touch & Hold Began
      let touchPoint = sender.location(in: sender.view)
      // Check if touch was for language menu button
      languageMenuPosition { keyFrame in
        if keyFrame.contains(touchPoint) {
          self.delegate?.menuKeyHeld(self)
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
      button.setTitleColor(.black, for: .disabled)
      button.setTitleColor(.black, for: .highlighted)

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
      let textSize = subKeyText.size(withAttributes: [NSAttributedStringKey.font: button.titleLabel?.font! as Any])
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
      button.tintColor = UIColor(red: 181.0 / 255.0, green: 181.0 / 255.0, blue: 181.0 / 255.0, alpha: 1.0)
      button.isEnabled = false
      return button
    }

    dismissKeyPreview()
    subKeysView = SubKeysView(keyFrame: subKeyAnchor, subKeys: subKeys)
    view.addSubview(subKeysView!)
    setPopupVisible(true)
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
      kbSize = size
      setOskWidth(Int(size.width))
      setOskHeight(Int(size.height))
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
      log.info("Current keyboard is set.")
      _ = Manager.shared.setKeyboard(keyboard)
    } else if let keyboard = Storage.active.userDefaults.userKeyboards?[safe: 0] {
      log.info("Using user's default keyboard.")
      _ = Manager.shared.setKeyboard(keyboard)
    } else {
      log.info("Using app-default keyboard.")
      _ = Manager.shared.setKeyboard(Defaults.keyboard)
    }
  }

  // MARK: - Show/hide views
  func reloadKeyboard() {
    webView!.loadFileURL(Storage.active.kmwURL, allowingReadAccessTo: Storage.active.baseDir)
  }

  @objc func showHelpBubble() {
    // Help bubble is always disabled for system-wide keyboard
    if Util.isSystemKeyboard || keyboardMenuView != nil {
      return
    }

    languageMenuPosition { keyFrame in
      self.showHelpBubble(at: keyFrame.origin)
    }
  }

  // TODO: The bulk of this should be moved to PopoverView
  private func showHelpBubble(at point: CGPoint) {
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
    helpText.textColor = UIColor.darkText
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
