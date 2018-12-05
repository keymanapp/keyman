//
//  UIResponder+Types.swift
//  KeymanEngine
//
//  Created by Joshua A. Horton on 2018-12-05.
//  Copyright © 2018 SIL International. All rights reserved.
//

import UIKit

// Replaces the old use of KeymanWebDelegate for tracking an app's active responder element.
public protocol KeymanResponder {
  /// Used internally and by API to hide the keyboard.
  func dismissKeyboard()

  /// Called to request display of the keyboard picker.
  /// Returns `false` if it cannot.
  func showKeyboardPicker() -> Bool
}

// We extend UIResponder so that both TextView and TextField have a common ancestor type
// we can utilize that has access to the required method `resignFirstResponder`.
extension UIResponder: KeymanResponder {
  public func dismissKeyboard() {
    // If not otherwise implemented (via extension with 'where'), just do the default first responder thing.
    // Our TextView and TextField have specific implementations.
    resignFirstResponder()
  }
  
  public func showKeyboardPicker() -> Bool {
    return false
  }
}
