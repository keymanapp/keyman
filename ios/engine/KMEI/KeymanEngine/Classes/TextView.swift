//
//  TextView.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import AudioToolbox
import UIKit

public class TextView: UITextView, UITextViewDelegate, UIInputViewAudioFeedback, KeymanWebDelegate {
  // viewController should be set to main view controller to enable keyboard picker.
  public var viewController: UIViewController?

  // Sets Keyman custom font (if any) to TextView instance on keyboard change event
  public var shouldSetCustomFontOnKeyboardChange = true
  public var isInputClickSoundEnabled = true

  private var delegateProxy: TextViewDelegateProxy!
  private var shouldUpdateKMText = false

  private var keyboardChangedObserver: NotificationObserver?

  // MARK: - Object Admin
  public convenience init() {
    self.init(frame: CGRect.zero)
  }

  public convenience init(frame: CGRect) {
    self.init(frame: frame, textContainer: nil)
  }

  public override init(frame: CGRect, textContainer: NSTextContainer?) {
    super.init(frame: frame, textContainer: textContainer)
    performCommonInit()
  }

  public required init?(coder aDecoder: NSCoder) {
    super.init(coder: aDecoder)
    performCommonInit()
  }

  private func performCommonInit() {
    delegateProxy = TextViewDelegateProxy(self)
    delegate = delegateProxy

    if #available(iOS 9.0, *) {
      inputAssistantItem.leadingBarButtonGroups = []
      inputAssistantItem.trailingBarButtonGroups = []
    }

    keyboardChangedObserver = NotificationCenter.default.addObserver(forName: Notifications.keyboardChanged,
                                                                     observer: self,
                                                                     function: TextView.keyboardChanged)
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
  public override weak var delegate: UITextViewDelegate? {
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
          "Trying to set TextView's delegate directly. Use setKeymanDelegate() instead.",
          checkDebugPrinting: true)
      }
      super.delegate = delegateProxy
    }
  }

  // MARK: - Public Methods

  // Use this TextViewDelegate instead of the normal UITextViewDelegate.
  //   - All of the normal UITextViewDelegate methods are supported.
  public func setKeymanDelegate(_ keymanDelegate: TextViewDelegate?) {
    delegateProxy.keymanDelegate = keymanDelegate
    Manager.shared.kmLog(
      "TextView: \(self.debugDescription) keymanDelegate set to: \(keymanDelegate.debugDescription)",
      checkDebugPrinting: true)
  }

  // Dismisses the keyboard if this textview is the first responder.
  //   - Use this instead of [resignFirstResponder] as it also resigns the Keyman keyboard's responders.
  public func dismissKeyboard() {
    Manager.shared.kmLog(
      "TextView: \(self.debugDescription) Dismissing keyboard. Was first responder:\(isFirstResponder)",
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
      Manager.shared.setSelectionRange(selectedRange, manually: false)
    }
  }

  // MARK: - KeymanWebDelegate
  func insertText(_ view: KeymanWebViewController, numCharsToDelete: Int, newText: String) {
    if Manager.shared.isSubKeysMenuVisible {
      return
    }

    if isInputClickSoundEnabled {
      AudioServicesPlaySystemSound(0x450)

      // Disable input click sound for 0.1 second to ensure it plays for single key stroke.
      isInputClickSoundEnabled = false
      perform(#selector(self.enableInputClickSound), with: nil, afterDelay: 0.1)
    }

    let textRange = selectedTextRange ?? UITextRange()
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

    // Workaround for iOS 7 UITextView scroll bug
    // TODO check if fixed
    perform(#selector(self.scroll(toShowSelection:)), with: self, afterDelay: 0.1)
    // Smaller delays are unreliable.
  }

  func hideKeyboard(_ view: KeymanWebViewController) {
    dismissKeyboard()
  }

  func menuKeyUp(_ view: KeymanWebViewController) {
    if let viewController = viewController {
      Manager.shared.showKeyboardPicker(in: viewController, shouldAddKeyboard: false)
    } else {
      Manager.shared.switchToNextKeyboard()
    }
  }

  // MARK: - UITextViewDelegate Hooks
  public override var selectedTextRange: UITextRange? {
    didSet {
      Manager.shared.setSelectionRange(selectedRange, manually: false)
    }
  }

  public func textViewDidChangeSelection(_ textView: UITextView) {
    // Workaround for iOS 7 UITextView scroll bug
    scroll(toCarret: textView)
    scrollRangeToVisible(textView.selectedRange)
  }

  public func textViewShouldBeginEditing(_ textView: UITextView) -> Bool {
    let leftToRightMark = "\u{200e}"
    let rightToLeftMark = "\u{200f}" // or "\u{202e}"
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

        if text.isEmpty || !text.hasPrefix(rightToLeftMark) {
          text = "\(rightToLeftMark)\(text!)"
        }
        makeTextWritingDirectionRightToLeft(nil)
      }
    } else {
      if textWD != .leftToRight {
        if textWD != .leftToRight {
          if text.hasPrefix(rightToLeftMark) {
            text = String(text.utf16.dropFirst())
          }

          if text.isEmpty || !text.hasPrefix(leftToRightMark) {
            text = "\(leftToRightMark)\(text!)"
          }
          makeTextWritingDirectionLeftToRight(nil)
        }
      }
    }

    return true
  }

  public func textViewDidBeginEditing(_ textView: UITextView) {
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

    Manager.shared.kmLog("TextView setFont: \(String(describing: font?.familyName))",
      checkDebugPrinting: true)

    // copy this textView's text to the webview
    Manager.shared.setText(text)
    Manager.shared.setSelectionRange(selectedRange, manually: false)
    Manager.shared.kmLog(
      "TextView: \(self.debugDescription) Became first responder. Value: \(text.debugDescription)",
      checkDebugPrinting: true)
  }

  public func textView(_ textView: UITextView, shouldChangeTextIn range: NSRange,
                       replacementText text: String) -> Bool {
    // Enable text update to catch copy/paste operations
    shouldUpdateKMText = true
    return true
  }

  public func textViewDidChange(_ textView: UITextView) {
    if shouldUpdateKMText {
      // Catches copy/paste operations
      Manager.shared.setText(textView.text)
      Manager.shared.setSelectionRange(textView.selectedRange, manually: false)
      shouldUpdateKMText = false
    }
  }

  public func textViewShouldEndEditing(_ textView: UITextView) -> Bool {
    if textView == self {
      resignFirstResponder()
    }
    return true
  }

  // MARK: - Keyman notifications
  private func keyboardChanged(_ kb: InstallableKeyboard) {
    if !shouldSetCustomFontOnKeyboardChange {
      return
    }

    // TODO: Get font name directly from keyboard
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

    Manager.shared.kmLog("TextView setFont: \(String(describing: font?.familyName))",
      checkDebugPrinting: true)
  }

  // MARK: iOS 7 TextView Scroll bug fix
  func scroll(toCarret textView: UITextView) {
    guard let range = textView.selectedTextRange else {
      return
    }
    var caretRect = textView.caretRect(for: range.end)
    caretRect.size.height += textView.textContainerInset.bottom
    textView.scrollRectToVisible(caretRect, animated: false)
  }

  @objc func scroll(toShowSelection textView: UITextView) {
    if textView.selectedRange.location < textView.text.count {
      return
    }

    var bottomOffset = CGPoint(x: 0, y: textView.contentSize.height - textView.bounds.size.height)
    if bottomOffset.y < 0 {
      bottomOffset.y = 0
    }
    textView.setContentOffset(bottomOffset, animated: true)
  }

  // MARK: - Private Methods
  @objc func enableInputClickSound() {
    isInputClickSoundEnabled = true
  }
}
