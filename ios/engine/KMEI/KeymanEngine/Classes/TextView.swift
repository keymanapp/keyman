//
//  TextView.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import AudioToolbox
import UIKit
import os.log

public class TextView: UITextView, KeymanResponder {
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

    inputAssistantItem.leadingBarButtonGroups = []
    inputAssistantItem.trailingBarButtonGroups = []
    
    self.inputView = Manager.shared.inputViewController.view

    keyboardChangedObserver = NotificationCenter.default.addObserver(forName: Notifications.keyboardChanged,
                                                                     observer: self,
                                                                     function: TextView.keyboardChanged)
  }

  // MARK: - Class Overrides
  public override var inputViewController: UIInputViewController? {
    get {
      return Manager.shared.inputViewController
    }

    set(inputViewController) {
      _ = inputViewController
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
        os_log("Trying to set TextView's delegate directly. Use setKeymanDelegate() instead.", log:KeymanEngineLogger.ui, type: .error)
     }
      super.delegate = delegateProxy
    }
  }

  // MARK: - Public Methods

  // Use this TextViewDelegate instead of the normal UITextViewDelegate.
  //   - All of the normal UITextViewDelegate methods are supported.
  public func setKeymanDelegate(_ keymanDelegate: TextViewDelegate?) {
    delegateProxy.keymanDelegate = keymanDelegate
    let message = "TextView: \(self.hashValue) keymanDelegate set to: \(keymanDelegate.debugDescription)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, message)
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

     Manager.shared.inputViewController.setContextState(text: self.text, range: selectedRange)
     Manager.shared.resetContext()
    }
  }

  public override var selectedTextRange: UITextRange? {
    didSet {
      Manager.shared.setContextState(text: self.text, range: selectedRange)
      Manager.shared.resetContext()
    }
  }

  // MARK: - Keyman notifications
  private func keyboardChanged(_ kb: InstallableKeyboard) {
    if !shouldSetCustomFontOnKeyboardChange {
      return
    }

    let fontName = Manager.shared.fontNameForKeyboard(withFullID: kb.fullID)
    let fontSize = font?.pointSize ?? UIFont.systemFontSize
    if let fontName = fontName {
      font = UIFont(name: fontName, size: fontSize)
    } else {
      font = UIFont.systemFont(ofSize: fontSize)
    }

    let message = "TextView: \(self.hashValue) setFont: \(font?.familyName ?? "nil")"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, message)
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

extension KeymanResponder where Self: TextView {
  // Dismisses the keyboard if this textview is the first responder.
  //   - Use this instead of [resignFirstResponder] as it also resigns the Keyman keyboard's responders.
  public func dismissKeyboard() {
    let message = "TextView: \(self.hashValue) dismissing keyboard. Was first responder: \(isFirstResponder)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, message)
    resignFirstResponder()
    Manager.shared.inputViewController.endEditing(true)
  }
  
  public func summonKeyboard() {
    becomeFirstResponder()
  }

  public func showKeyboardPicker() -> Bool {
    if let viewController = viewController {
      Manager.shared.showKeyboardPicker(in: viewController, shouldAddKeyboard: false)
      return true
    } else {
      return false
    }
  }
}

//// MARK: - UITextViewDelegate
extension TextView: UITextViewDelegate {
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
    if let keyboard = Manager.shared.currentKeyboard {
      isRTL = keyboard.isRTL
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
    Manager.shared.currentResponder = self

    let fontName: String?
    if let id = Manager.shared.currentKeyboardID {
      fontName = Manager.shared.fontNameForKeyboard(withFullID: id)
    } else {
      fontName = nil
    }
    let fontSize = font?.pointSize ?? UIFont.systemFontSize
    if let fontName = fontName {
      font = UIFont(name: fontName, size: fontSize)
    } else {
      font = UIFont.systemFont(ofSize: fontSize)
    }

    let messageOne = "TextView: \(self.hashValue) setFont: \(font?.familyName ?? "nil")"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, messageOne)

    let messageTwo = "TextView: \(self.hashValue) Became first responder. Value: \(String(describing: text))"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, messageTwo)
  }

  public func textViewShouldEndEditing(_ textView: UITextView) -> Bool {
    if textView == self {
      resignFirstResponder()
    }
    return true
  }
}
