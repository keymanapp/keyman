//
//  KeymanTextField.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import AudioToolbox
import UIKit

public class KeymanTextField: UITextField, UITextFieldDelegate {
  // viewController should be set to main view controller to enable keyboard picker.
  public var viewController: UIViewController?

  // Sets Keyman custom font (if any) to KeymanTextField instance on keyboard change event
  public var shouldSetCustomFontOnKeyboardChange = true
  public var isInputClickSoundEnabled = true

  private var delegateProxy: KMTextFieldDelegateProxy?
  private var shouldUpdateKMText = false

  // MARK: - Object Admin
  deinit {
    NotificationCenter.default.removeObserver(self)
    delegate = nil
  }

  public convenience init() {
    self.init(frame: CGRect.zero)
  }

  public override init(frame: CGRect) {
    super.init(frame: frame)

    delegateProxy = KMTextFieldDelegateProxy()
    delegate = delegateProxy

    if #available(iOS 9.0, *) {
      inputAssistantItem.leadingBarButtonGroups = []
      inputAssistantItem.trailingBarButtonGroups = []
    }

    KMManager.sharedInstance() // Preload webview keyboard

    NotificationCenter.default.addObserver(self, selector: #selector(self.textFieldTextDidChange),
                                           name: .UITextFieldTextDidChange, object: self)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardChanged),
                                           name: NSNotification.Name.keymanKeyboardChanged, object: nil)
  }

  public required init?(coder aDecoder: NSCoder) {
    super.init(coder: aDecoder)

    delegateProxy = KMTextFieldDelegateProxy(self)
    delegate = delegateProxy

    if #available(iOS 9.0, *) {
      inputAssistantItem.leadingBarButtonGroups = []
      inputAssistantItem.trailingBarButtonGroups = []
    }

    KMManager.sharedInstance() // Preload webview keyboard

    NotificationCenter.default.addObserver(self, selector: #selector(self.textFieldTextDidChange),
                                           name: .UITextFieldTextDidChange, object: self)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardChanged),
                                           name: NSNotification.Name.keymanKeyboardChanged, object: nil)
  }

  // MARK: - Class Overrides
  public override var inputView: UIView? {
    get {
      KMManager.sharedInstance().webDelegate = self
      return KMManager.inputView()
    }

    set(inputView) {
      super.inputView = inputView
    }
  }

  // Prevent the delegate being set to anything but the delegateProxy
  public override weak var delegate: UITextFieldDelegate? {
    get {
      return super.delegate
    }

    set(delegate) {
      // Allow clearing of delegate
      guard let delegate = delegate else {
        super.delegate = nil
        return
      }

      if delegate !== delegateProxy {
        KMManager.sharedInstance().kmLog(
          "Trying to set KeymanTextField's delegate directly. Use setKeymanDelegate() instead.",
          checkDebugPrinting: true)
      }
      super.delegate = delegateProxy
    }
  }

  // MARK: - Public Methods

  // Use this KMTextFieldDelegate instead of the normal UITextFieldDelegate.
  // All of the normal UITextFieldDelegate methods are supported.
  @objc public func setKeymanDelegate(_ keymanDelegate: KMTextFieldDelegate?) {
    delegateProxy?.keymanDelegate = keymanDelegate
    KMManager.sharedInstance().kmLog(
      "KeymanTextField: \(self.debugDescription) keymanDelegate set to: \(keymanDelegate.debugDescription)",
      checkDebugPrinting: true)
  }

  // Dismisses the keyboard if this textview is the first responder.
  //   - Use this instead of [resignFirstResponder] as it also resigns the Keyman keyboard's responders.
  @objc public func dismissKeyboard() {
    KMManager.sharedInstance().kmLog(
      "KeymanTextField: \(self.debugDescription) Dismissing keyboard. Was first responder:\(isFirstResponder)",
      checkDebugPrinting: true)
    resignFirstResponder()
    KMManager.inputView().endEditing(true)
  }

  public override var text: String! {
    get {
      return super.text ?? ""
    }

    set(text) {
      if let text = text {
        super.text = text
      } else {
        super.text = ""
      }

      KMManager.setKMText(self.text)
      let textRange = selectedTextRange!
      let newRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                             length: offset(from: textRange.start, to: textRange.end))
      KMManager.setKMSelectionRange(newRange, manually: false)
    }
  }

  // MARK: - KMWebViewDelegate
  @objc public func updatedFragment(_ fragment: String) {
    if fragment.contains("insertText") {
      processInsertText(fragment)
    } else if fragment.contains("hideKeyboard") {
      dismissKeyboard()
    } else if fragment.contains("menuKeyUp") {
      if let viewController = viewController {
        KMManager.sharedInstance().showKeyboardPicker(in: viewController, shouldAddKeyboard: false)
      } else {
        KMManager.sharedInstance().switchToNextKeyboard()
      }
    }
  }

  private func processInsertText(_ fragment: String) {
    if KMManager.sharedInstance().isSubKeysMenuVisible() {
      return
    }

    if isInputClickSoundEnabled {
      AudioServicesPlaySystemSound(0x450)

      // Disable input click sound for 0.1 second to ensure it plays for single key stroke.
      isInputClickSoundEnabled = false
      perform(#selector(self.enableInputClickSound), with: nil, afterDelay: 0.1)
    }

    // TODO: Refactor duplicate logic in KeymanInputViewController and KMTextView
    let dnRange = fragment.range(of: "+dn=")!
    let sRange = fragment.range(of: "+s=")!

    let dn = Int(fragment[dnRange.upperBound..<sRange.lowerBound])!
    let s = fragment[sRange.upperBound...]

    let hexStrings: [String]
    if !s.isEmpty {
      hexStrings = s.components(separatedBy: ",")
    } else {
      hexStrings = []
    }

    let codeUnits = hexStrings.map { hexString -> UInt16 in
      let scanner = Scanner(string: hexString)
      var codeUnit: UInt32 = 0
      scanner.scanHexInt32(&codeUnit)
      return UInt16(codeUnit)
    }

    let text = String(utf16CodeUnits: codeUnits, count: codeUnits.count)

    let textRange = selectedTextRange!
    let selRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                           length: offset(from: textRange.start, to: textRange.end))

    if dn <= 0 {
      if selRange.length == 0 {
        insertText(text)
      } else {
        self.text = (self.text! as NSString).replacingCharacters(in: selRange, with: text)
      }
    } else {
      if s.isEmpty {
        for _ in 0..<dn {
          deleteBackward()
        }
      } else {
        if selRange.length == 0 {
          for _ in 0..<dn {
            deleteBackward()
          }
          insertText(text)
        } else {
          self.text = (self.text! as NSString).replacingCharacters(in: selRange, with: text)
        }
      }
    }
  }

  // MARK: - UITextFieldDelegate Hooks
  public override var selectedTextRange: UITextRange? {
    didSet(range) {
      guard let range = range else {
        return
      }
      let newRange = NSRange(location: offset(from: beginningOfDocument, to: range.start),
                             length: offset(from: range.start, to: range.end))
      KMManager.setKMSelectionRange(newRange, manually: false)
    }
  }

  public func textField(_ textField: UITextField, shouldChangeCharactersIn range: NSRange,
                        replacementString string: String) -> Bool {
    shouldUpdateKMText = true // Enable text update to catch copy/paste operations
    return true
  }

  @objc public func textFieldTextDidChange(_ notification: Notification) {
    if shouldUpdateKMText {
      // Catches copy/paste operations
      KMManager.setKMText(text)
      let textRange = selectedTextRange!
      let newRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                             length: offset(from: textRange.start, to: textRange.end))
      KMManager.setKMSelectionRange(newRange, manually: false)
      shouldUpdateKMText = false
    }
  }

  public func textFieldShouldClear(_ textField: UITextField) -> Bool {
    if textField == self {
      KMManager.clearText()
    }
    return true
  }

  public func textFieldShouldBeginEditing(_ textField: UITextField) -> Bool {
    let leftToRightMark = "\u{200e}"
    let rightToLeftOverride = "\u{202e}"
    let keyboardID = KMManager.sharedInstance().keyboardID
    let languageID = KMManager.sharedInstance().languageID
    let textWD = baseWritingDirection(for: beginningOfDocument, in: .forward)

    if KMManager.sharedInstance().isRTLKeyboard(withID: keyboardID, languageID: languageID) {
      if textWD != .rightToLeft {
        if text.hasPrefix(leftToRightMark) {
          text = String(text.utf16.dropFirst())
        }

        if text.isEmpty || !text.hasPrefix(rightToLeftOverride) {
          text = "\(rightToLeftOverride)\(text!)"
        }
        makeTextWritingDirectionRightToLeft(nil)
      }
    } else {
      if textWD != .leftToRight {
        if text.hasPrefix(rightToLeftOverride) {
          text = String(text.utf16.dropFirst())
        }

        if text.isEmpty || !text.hasPrefix(leftToRightMark) {
          text = "\(leftToRightMark)\(text!)"
        }
        makeTextWritingDirectionLeftToRight(nil)
      }
    }
    return true
  }

  public func textFieldDidBeginEditing(_ textField: UITextField) {
    KMManager.sharedInstance().webDelegate = self

    let keyboardID = KMManager.sharedInstance().keyboardID
    let languageID = KMManager.sharedInstance().languageID
    let fontName = KMManager.sharedInstance().fontNameForKeyboard(withID: keyboardID, languageID: languageID)
    let fontSize = font?.pointSize ?? UIFont.systemFontSize
    if let fontName = fontName {
      font = UIFont(name: fontName, size: fontSize)
    } else {
      font = UIFont.systemFont(ofSize: fontSize)
    }

    KMManager.sharedInstance().kmLog("TextField setFont: \(String(describing: font?.familyName))",
      checkDebugPrinting: true)

    // copy this textField's text to the webview
    KMManager.setKMText(text)
    let textRange = selectedTextRange!
    let newRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                           length: offset(from: textRange.start, to: textRange.end))
    KMManager.setKMSelectionRange(newRange, manually: false)
    KMManager.sharedInstance().kmLog(
      "KeymanTextField: \(self.debugDescription) Became first responder. Value: \(String(describing: text))",
      checkDebugPrinting: true)
  }

  public func textFieldShouldEndEditing(_ textField: UITextField) -> Bool {
    if textField == self {
      resignFirstResponder()
    }

    return true
  }

  // MARK: - Keyman notifications
  @objc public func keyboardChanged(_ notification: Notification) {
    if !shouldSetCustomFontOnKeyboardChange {
      return
    }

    let kbInfo = notification.userInfo?[kKeymanKeyboardInfoKey] as? [AnyHashable: Any]
    let keyboardID = kbInfo?[kKeymanKeyboardIdKey] as? String
    let languageID = kbInfo?[kKeymanLanguageIdKey] as? String
    let fontName = KMManager.sharedInstance().fontNameForKeyboard(withID: keyboardID, languageID: languageID)
    let fontSize = font?.pointSize ?? UIFont.systemFontSize
    if let fontName = fontName {
      font = UIFont(name: fontName, size: fontSize)
    } else {
      font = UIFont.systemFont(ofSize: fontSize)
    }

    if isFirstResponder {
      resignFirstResponder()
      becomeFirstResponder()
    }
    KMManager.sharedInstance().kmLog(
      "KeymanTextField \(self.debugDescription) setFont: \(font!.familyName)", checkDebugPrinting: true)
  }

  @objc public func enableInputClickSound() {
    isInputClickSoundEnabled = true
  }
}
