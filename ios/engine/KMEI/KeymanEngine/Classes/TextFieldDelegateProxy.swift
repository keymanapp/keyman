//
//  TextFieldDelegateProxy.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//
//  Proxies delegate messages for a TextField.
//    This allows the TextField to hook into these calls while allowing a developer to still
//    use the delegate as normal (albeit with a different name: 'keymanDelegate')
//
//  This class is required because at the time of writing, setting a UITextField as it's
//  own delegate caused an infinite loop in Apple's code, repeatedly calling the selectionDidChange callback
//    See: http://lists.apple.com/archives/cocoa-dev/2010/Feb/msg01391.html

import UIKit

public protocol TextFieldDelegate: UITextFieldDelegate { }

class TextFieldDelegateProxy: NSObject, UITextFieldDelegate {
  weak var keymanDelegate: TextFieldDelegate?
  private unowned let textField: UITextFieldDelegate
  
  init(_ textField: TextField) {
    self.textField = textField
    super.init()
  }
  
  // MARK: - UITextFieldDelegate
  
  // NOTE: Return values from the TextField hooks are ignored
  func textFieldShouldBeginEditing(_ textField: UITextField) -> Bool {
    _ = self.textField.textFieldShouldBeginEditing?(textField)
    return keymanDelegate?.textFieldShouldBeginEditing?(textField) ?? true
  }
  
  func textFieldDidBeginEditing(_ textField: UITextField) {
    self.textField.textFieldDidBeginEditing?(textField)
    keymanDelegate?.textFieldDidBeginEditing?(textField)
  }
  
  func textFieldShouldEndEditing(_ textField: UITextField) -> Bool {
    _ = self.textField.textFieldShouldEndEditing?(textField)
    return keymanDelegate?.textFieldShouldEndEditing?(textField) ?? true
  }
  
  func textFieldDidEndEditing(_ textField: UITextField) {
    self.textField.textFieldDidEndEditing?(textField)
    keymanDelegate?.textFieldDidEndEditing?(textField)
  }
  
  @available(iOS 10.0, *)
  func textFieldDidEndEditing(_ textField: UITextField, reason: UITextField.DidEndEditingReason) {
    self.textField.textFieldDidEndEditing?(textField, reason: reason)
    keymanDelegate?.textFieldDidEndEditing?(textField, reason: reason)
  }
  
  func textField(_ textField: UITextField, shouldChangeCharactersIn range: NSRange,
                 replacementString string: String) -> Bool {
    _ = self.textField.textField?(textField, shouldChangeCharactersIn: range, replacementString: string)
    return keymanDelegate?.textField?(textField, shouldChangeCharactersIn: range, replacementString: string) ?? true
  }
  
  func textFieldShouldClear(_ textField: UITextField) -> Bool {
    _ = self.textField.textFieldShouldClear?(textField)
    return keymanDelegate?.textFieldShouldClear?(textField) ?? true
  }
  
  func textFieldShouldReturn(_ textField: UITextField) -> Bool {
    _ = self.textField.textFieldShouldReturn?(textField)
    return keymanDelegate?.textFieldShouldReturn?(textField) ?? true
  }
}
