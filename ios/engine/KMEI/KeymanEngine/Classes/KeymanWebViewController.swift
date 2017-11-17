//
//  KeymanWebViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-31.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit
import WebKit

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
  weak var delegate: KeymanWebDelegate?
  private var useSpecialFont = false

  // Views
  var webView: WKWebView!
  private var helpBubbleView: PopoverView?
  private var keyPreviewView: KeyPreviewView?
  private var subKeysView: SubKeysView?
  private var keyboardMenuView: KeyboardMenuView?

  // Arrays
  private var subKeyIDs: [String] = []
  private var subKeyTexts: [String] = []
  private var subKeys: [UIButton] = []

  private var subKeyAnchor = CGRect.zero
  private var lastKeyboardSize: CGSize = .zero

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
    webView.isOpaque = false
    webView.backgroundColor = UIColor.clear
    webView.navigationDelegate = self
    webView.scrollView.isScrollEnabled = false

    view = webView

    // Set UILongPressGestureRecognizer to show sub keys
    let hold = UILongPressGestureRecognizer(target: self, action: #selector(self.holdAction))
    hold.minimumPressDuration = 0.5
    hold.delegate = self
    view.addGestureRecognizer(hold)
    reloadKeyboard()
  }

  override func viewWillAppear(_ animated: Bool) {
    view.frame = CGRect(origin: .zero, size: keyboardSize)
    super.viewWillAppear(animated)
  }
}

// MARK: - JavaScript functions
extension KeymanWebViewController {
  func languageMenuPosition(_ completion: @escaping (CGRect) -> Void) {
    webView.evaluateJavaScript("langMenuPos();") { result, _ in
      guard let result = result as? String, !result.isEmpty else {
        return
      }
      let components = result.components(separatedBy: ",")
      let x = CGFloat(Float(components[0])!)
      let y = CGFloat(Float(components[1])!)
      let w = CGFloat(Float(components[2])!)
      let h = CGFloat(Float(components[3])!)
      let isPad = UIDevice.current.userInterfaceIdiom == .pad
      let adjY: CGFloat = isPad ? -0.5 : -1.0
      let frame = CGRect(x: x - w / 2.0, y: y - adjY, width: w, height: h)
      completion(frame)
    }
  }

  // FIXME: text is unused in the JS
  func executePopupKey(id: String, text: String) {
    webView.evaluateJavaScript("executePopupKey('\(id)','\(text)');", completionHandler: nil)
  }

  func setOskWidth(_ width: Int) {
    webView.evaluateJavaScript("setOskWidth(\(width));", completionHandler: nil)
  }

  func setOskHeight(_ height: Int) {
    webView.evaluateJavaScript("setOskHeight(\(height));", completionHandler: nil)
  }

  func setPopupVisible(_ visible: Bool) {
    webView.evaluateJavaScript("popupVisible(\(visible));", completionHandler: nil)
  }

  func setCursorRange(_ range: NSRange) {
    if range.location != NSNotFound {
      webView.evaluateJavaScript("setCursorRange(\(range.location),\(range.length));", completionHandler: nil)
    }
  }

  func setText(_ text: String?) {
    var text = text ?? ""
    text = text.replacingOccurrences(of: "\\", with: "\\\\")
    text = text.replacingOccurrences(of: "'", with: "\\'")
    text = text.replacingOccurrences(of: "\n", with: "\\n")
    webView.evaluateJavaScript("setKeymanVal('\(text)');", completionHandler: nil)
  }

  func setDeviceType(_ idiom: UIUserInterfaceIdiom) {
    let type: String
    switch idiom {
    case .phone:
      type = "AppleMobile"
    case .pad:
      type = "AppleTablet"
    default:
      Manager.shared.kmLog("Unexpected interface idiom: \(idiom)", checkDebugPrinting: false)
      return
    }
    webView.evaluateJavaScript("setDeviceType('\(type)');", completionHandler: nil)
  }

  func setKeyboard(id: String,
                   name: String,
                   languageID: String,
                   languageName: String,
                   version: String,
                   font: String,
                   oskFont: String) {
    let escapedID = id.replacingOccurrences(of: "'", with: "\\'")
    let escapedName = name.replacingOccurrences(of: "'", with: "\\'")
    let escapedLanguageID = languageID.replacingOccurrences(of: "'", with: "\\'")
    let escapedLanguageName = languageName.replacingOccurrences(of: "'", with: "\\'")
    let escapedVersion = version.replacingOccurrences(of: "'", with: "\\'")

    let jsString = """
    setKeymanLanguage('\(escapedName)','\(escapedID)','\(escapedLanguageName)','\(escapedLanguageID)',\
    '\(escapedVersion)',\(font),\(oskFont));
    """
    webView.evaluateJavaScript(jsString, completionHandler: nil)
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

      let dn = Int(fragment[dnRange.upperBound..<sRange.lowerBound])!
      let s = fragment[sRange.upperBound...]

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

      let frame = keyFrame(x: x, y: y, w: w, h: h)
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
      let frame = keyFrame(x: x, y: y, w: w, h: h)

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
          Manager.shared.kmLog("Unexpected subkey key: \(key)", checkDebugPrinting: false)
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
      delegate?.hideKeyboard(self)
    } else if fragment.hasPrefix("ios-log:#iOS#") {
      let message = fragment.dropFirst(13)
      Manager.shared.kmLog("KMW Log: \(message)", checkDebugPrinting: true)
    } else {
      Manager.shared.kmLog("Unexpected KMW event: \(fragment)", checkDebugPrinting: false)
    }
  }

  private func keyFrame(x: CGFloat, y: CGFloat, w: CGFloat, h: CGFloat) -> CGRect {
    let isPad = UIDevice.current.userInterfaceIdiom == .pad
    let adjY: CGFloat = isPad ? -0.5 : -1.0
    return CGRect(x: x - w / 2.0, y: y - adjY, width: w, height: h)
  }
}

// MARK: - WKNavigationDelegate
extension KeymanWebViewController: WKNavigationDelegate {
  func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
    guard let url = webView.url else {
      return
    }
    guard url.lastPathComponent == Resources.kmwFileName && (url.fragment?.isEmpty ?? true) else {
      return
    }
    keyboardLoaded(self)
    delegate?.keyboardLoaded(self)
  }
}

// MARK: - KeymanWebDelegate
extension KeymanWebViewController: KeymanWebDelegate {
  func insertText(_ view: KeymanWebViewController, numCharsToDelete: Int, newText: String) {
    dismissHelpBubble()
    Manager.shared.isKeymanHelpOn = false
  }

  func showKeyPreview(_ view: KeymanWebViewController, keyFrame: CGRect, preview: String) {
    if UIDevice.current.userInterfaceIdiom == .pad
      || (Util.isSystemKeyboard && !Manager.shared.isSystemKeyboardTopBarEnabled)
      || isSubKeysMenuVisible {
      return
    }

    dismissKeyPreview()
    clearSubKeyArrays()

    keyPreviewView = KeyPreviewView(frame: keyFrame)

    keyPreviewView!.setLabelText(preview)
    let keyboardID = Manager.shared.keyboardID!
    let languageID = Manager.shared.languageID!
    var oskFontName = Manager.shared.oskFontNameForKeyboard(withID: keyboardID, languageID: languageID)
    oskFontName = oskFontName ?? Manager.shared.fontNameForKeyboard(withID: keyboardID, languageID: languageID)
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
    let isPad = UIDevice.current.userInterfaceIdiom == .pad
    let fontSize = isPad ? UIFont.buttonFontSize * 2 : UIFont.buttonFontSize

    let keyboardID = Manager.shared.keyboardID!
    let languageID = Manager.shared.languageID!
    let oskFontName = Manager.shared.oskFontNameForKeyboard(withID: keyboardID, languageID: languageID)
      ?? Manager.shared.fontNameForKeyboard(withID: keyboardID, languageID: languageID)

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

      if useSpecialFont {
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
    let isSystemKeyboard = Util.isSystemKeyboard
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

  func resizeKeyboard() {
    let newSize = keyboardSize
    if Util.isSystemKeyboard && lastKeyboardSize == newSize {
      return
    }
    lastKeyboardSize = newSize

    view.frame = CGRect(origin: .zero, size: newSize)

    // Workaround for WKWebView bug with landscape orientation
    // TODO: Check if still necessary and if there's a better solution
    if Util.isSystemKeyboard {
      perform(#selector(self.resizeDelay), with: self, afterDelay: 1.0)
    }

    var oskHeight = Int(newSize.height)
    oskHeight -= oskHeight % (Util.isSystemKeyboard ? 10 : 20)

    setOskWidth(Int(newSize.width))
    setOskHeight(oskHeight)
  }

  @objc func resizeDelay() {
    // + 1000 to work around iOS bug with resizing on landscape orientation. Technically we only
    // need this for landscape but it doesn't hurt to do it with both. 1000 is a big number that
    // should hopefully work on all devices.
    let kbWidth = keyboardWidth
    let kbHeight = keyboardHeight
    view.frame = CGRect(x: 0.0, y: 0.0, width: kbWidth, height: kbHeight + 1000)
  }

  func resizeKeyboard(with orientation: UIInterfaceOrientation) {
    // TODO: Update to use new size instead of orientation since viewWillRotate() is deprecated
    // TODO: Refactor to use resizeKeyboard()
    let kbWidth = keyboardWidth
    let kbHeight = keyboardHeight(with: orientation)
    view.frame = CGRect(x: 0.0, y: 0.0, width: kbWidth, height: kbHeight)

    var oskHeight = Int(kbHeight)
    oskHeight -= oskHeight % (Util.isSystemKeyboard ? 10 : 20)

    setOskWidth(Int(kbWidth))
    setOskHeight(oskHeight)
  }

  // MARK: - Show/hide views
  func reloadKeyboard() {
    if #available(iOS 9.0, *) {
      webView.loadFileURL(Storage.active.kmwURL, allowingReadAccessTo: Storage.active.baseDir)
    } else {
      // WKWebView in iOS < 9 is missing loadFileURL().
      let request = URLRequest(url: Storage.active.kmwURL,
                               cachePolicy: .reloadIgnoringCacheData,
                               timeoutInterval: 60.0)
      webView.load(request)
    }
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
