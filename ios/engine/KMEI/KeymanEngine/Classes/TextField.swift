//
//  TextField.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import AudioToolbox
import UIKit
import os.log

public class TextField: UITextField, KeymanResponder {
  // viewController should be set to main view controller to enable keyboard picker.
  public var viewController: UIViewController?
  
  // Sets Keyman custom font (if any) to TextField instance on keyboard change event
  public var shouldSetCustomFontOnKeyboardChange = true
  public var isInputClickSoundEnabled = true
  
  private var delegateProxy: TextFieldDelegateProxy!
  
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
    
    inputAssistantItem.leadingBarButtonGroups = []
    inputAssistantItem.trailingBarButtonGroups = []
    
    self.inputView = Manager.shared.inputViewController.view
    
    keyboardChangedObserver = NotificationCenter.default.addObserver(forName: Notifications.keyboardChanged,
                                                                     observer: self,
                                                                     function: TextField.keyboardChanged)
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
        os_log("Trying to set TextField's delegate directly. Use setKeymanDelegate() instead.", log:KeymanEngineLogger.ui, type: .error)
      }
      super.delegate = delegateProxy
    }
  }
  
  // MARK: - Public Methods
  
  // Use this TextFieldDelegate instead of the normal UITextFieldDelegate.
  // All of the normal UITextFieldDelegate methods are supported.
  public func setKeymanDelegate(_ keymanDelegate: TextFieldDelegate?) {
    delegateProxy.keymanDelegate = keymanDelegate
    let message = "TextField: \(self.hashValue) keymanDelegate set to: \(keymanDelegate.debugDescription)"
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
      
      let textRange = selectedTextRange ?? UITextRange()
      let newRange = NSRange(location: offset(from: beginningOfDocument, to: textRange.start),
                             length: offset(from: textRange.start, to: textRange.end))
      
      Manager.shared.setContextState(text: self.text, range: newRange)
      Manager.shared.resetContext()
    }
  }
  
  public override var selectedTextRange: UITextRange? {
    didSet(range) {
      guard let range = range else {
        return
      }
      let newRange = NSRange(location: offset(from: beginningOfDocument, to: range.start),
                             length: offset(from: range.start, to: range.end))
      
      Manager.shared.setContextState(text: self.text, range: newRange)
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
    
    let message = "TextField \(self.hashValue) setFont: \(font?.familyName ?? "nil")"
    os_log("%{public}s", log:KeymanEngineLogger.settings, type: .debug, message)
  }
  
  @objc func enableInputClickSound() {
    isInputClickSoundEnabled = true
  }
}

extension KeymanResponder where Self: TextField {
  // Dismisses the keyboard if this textview is the first responder.
  //   - Use this instead of [resignFirstResponder] as it also resigns the Keyman keyboard's responders.
  public func dismissKeyboard() {
    let message = "TextField: \(self.hashValue) dismissing keyboard. Was first responder: \(isFirstResponder)"
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

// MARK: - UITextFieldDelegate
extension TextField: UITextFieldDelegate {
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
    
    let messageOne = "TextField: \(self.hashValue) setFont: \(font?.familyName ?? "nil")"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, messageOne)
    
    let messageTwo = "TextField: \(self.hashValue) Became first responder. Value: \(String(describing: text))"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, messageTwo)
  }
  
  public func textFieldShouldEndEditing(_ textField: UITextField) -> Bool {
    if textField == self {
      resignFirstResponder()
    }
    
    return true
  }
}
