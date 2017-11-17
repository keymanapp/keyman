//
//  TextField.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import AudioToolbox
import UIKit

public class TextField: UITextField, UITextFieldDelegate, KeymanWebDelegate {
  // viewController should be set to main view controller to enable keyboard picker.
  public var viewController: UIViewController?

  // Sets Keyman custom font (if any) to TextField instance on keyboard change event
  public var shouldSetCustomFontOnKeyboardChange = true
  public var isInputClickSoundEnabled = true

  private var delegateProxy: TextFieldDelegateProxy!
  private var shouldUpdateKMText = false

  private var keyboardChangedObserver: NotificationObserver?

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
    performCommonInit()
  }

  public required init?(coder aDecoder: NSCoder) {
    super.init(coder: aDecoder)
    performCommonInit()
  }

  private func performCommonInit() {
    delegateProxy = TextFieldDelegateProxy(self)
    delegate = delegateProxy

    if #available(iOS 9.0, *) {
      inputAssistantItem.leadingBarButtonGroups = []
      inputAssistantItem.trailingBarButtonGroups = []
    }

    NotificationCenter.default.addObserver(self, selector: #selector(self.textFieldTextDidChange),
                                           name: .UITextFieldTextDidChange, object: self)
    keyboardChangedObserver = NotificationCenter.default.addObserver(forName: Notifications.keyboardChanged,
                                                                     observer: self,
                                                                     function: TextField.keyboardChanged)
  }

  // MARK: - Class Overrides
  public override var inputView: UIView? {
    get {
      Manager.shared.keymanWebDelegate = self
      return Manager.shared.keymanWeb.view
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
        Manager.shared.kmLog(
          "Trying to set TextField's delegate directly. Use setKeymanDelegate() instead.",
          checkDebugPrinting: true)
      }
      super.delegate = delegateProxy
    }
  }

  // MARK: - Public Methods

  // Use this TextFieldDelegate instead of the normal UITextFieldDelegate.
  // All of the normal UITextFieldDelegate methods are supported.
  public func setKeymanDelegate(_ keymanDelegate: TextFieldDelegate?) {
    delegateProxy.keymanDelegate = keymanDelegate
    Manager.shared.kmLog(
      "TextField: \(self.debugDescription) keymanDelegate set to: \(keymanDelegate.debugDescription)",
      checkDebugPrinting: true)
  }

  // Dismisses the keyboard if this textview is the first responder.
  //   - Use this instead of [resignFirstResponder] as it also resigns the Keyman keyboard's responders.
  public func dismissKeyboard() {
    Manager.shared.kmLog(
      "TextField: \(self.debugDescription) Dismissing keyboard. Was first responder:\(isFirstResponder)",
      checkDebugPrinting: true)
    resignFirstResponder()
    Manager.shared.keymanWeb.view.endEditing(true)
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

      Manager.shared.setText(self.text)
      let textRange = selectedTextRange ?? UITextRange()
      let newRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                             length: offset(from: textRange.start, to: textRange.end))
      Manager.shared.setSelectionRange(newRange, manually: false)
    }
  }

  // MARK: - KeymanWebViewDelegate
  func insertText(_ keymanWeb: KeymanWebViewController, numCharsToDelete: Int, newText: String) {
    if Manager.shared.isSubKeysMenuVisible {
      return
    }

    if isInputClickSoundEnabled {
      AudioServicesPlaySystemSound(0x450)

      // Disable input click sound for 0.1 second to ensure it plays for single key stroke.
      isInputClickSoundEnabled = false
      perform(#selector(self.enableInputClickSound), with: nil, afterDelay: 0.1)
    }

    let textRange = selectedTextRange!
    let selRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                           length: offset(from: textRange.start, to: textRange.end))

    if selRange.length != 0 {
      self.text = (self.text! as NSString).replacingCharacters(in: selRange, with: newText)
    } else {
      for _ in 0..<numCharsToDelete {
        deleteBackward()
      }
      insertText(newText)
    }
  }

  func hideKeyboard(_ keymanWeb: KeymanWebViewController) {
    dismissKeyboard()
  }

  func menuKeyUp(_ keymanWeb: KeymanWebViewController) {
    if let viewController = viewController {
      Manager.shared.showKeyboardPicker(in: viewController, shouldAddKeyboard: false)
    } else {
      Manager.shared.switchToNextKeyboard()
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
      Manager.shared.setSelectionRange(newRange, manually: false)
    }
  }

  public func textField(_ textField: UITextField, shouldChangeCharactersIn range: NSRange,
                        replacementString string: String) -> Bool {
    shouldUpdateKMText = true // Enable text update to catch copy/paste operations
    return true
  }

  @objc func textFieldTextDidChange(_ notification: Notification) {
    if shouldUpdateKMText {
      // Catches copy/paste operations
      Manager.shared.setText(text)
      let textRange = selectedTextRange!
      let newRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                             length: offset(from: textRange.start, to: textRange.end))
      Manager.shared.setSelectionRange(newRange, manually: false)
      shouldUpdateKMText = false
    }
  }

  public func textFieldShouldClear(_ textField: UITextField) -> Bool {
    if textField == self {
      Manager.shared.clearText()
    }
    return true
  }

  public func textFieldShouldBeginEditing(_ textField: UITextField) -> Bool {
    let leftToRightMark = "\u{200e}"
    let rightToLeftOverride = "\u{202e}"
    let textWD = baseWritingDirection(for: beginningOfDocument, in: .forward)

    let isRTL: Bool
    if let keyboardID = Manager.shared.keyboardID,
      let languageID = Manager.shared.languageID {
      isRTL = Manager.shared.isRTLKeyboard(withID: keyboardID, languageID: languageID) ?? false
    } else {
      isRTL = false
    }

    if isRTL {
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
    Manager.shared.keymanWebDelegate = self

    let fontName: String?
    if let keyboardID = Manager.shared.keyboardID,
      let languageID = Manager.shared.languageID {
      fontName = Manager.shared.fontNameForKeyboard(withID: keyboardID, languageID: languageID)
    } else {
      fontName = nil
    }
    let fontSize = font?.pointSize ?? UIFont.systemFontSize
    if let fontName = fontName {
      font = UIFont(name: fontName, size: fontSize)
    } else {
      font = UIFont.systemFont(ofSize: fontSize)
    }

    Manager.shared.kmLog("TextField setFont: \(String(describing: font?.familyName))",
      checkDebugPrinting: true)

    // copy this textField's text to the webview
    Manager.shared.setText(text)
    let textRange = selectedTextRange!
    let newRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                           length: offset(from: textRange.start, to: textRange.end))
    Manager.shared.setSelectionRange(newRange, manually: false)
    Manager.shared.kmLog(
      "TextField: \(self.debugDescription) Became first responder. Value: \(String(describing: text))",
      checkDebugPrinting: true)
  }

  public func textFieldShouldEndEditing(_ textField: UITextField) -> Bool {
    if textField == self {
      resignFirstResponder()
    }

    return true
  }

  // MARK: - Keyman notifications
  private func keyboardChanged(_ kb: InstallableKeyboard) {
    if !shouldSetCustomFontOnKeyboardChange {
      return
    }

    // TODO: Get font name directly from keyboard object
    let fontName = Manager.shared.fontNameForKeyboard(withID: kb.id, languageID: kb.languageID)
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
    Manager.shared.kmLog(
      "TextField \(self.debugDescription) setFont: \(font!.familyName)", checkDebugPrinting: true)
  }

  @objc func enableInputClickSound() {
    isInputClickSoundEnabled = true
  }
}
