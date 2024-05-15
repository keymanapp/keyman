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
  /// Used to unhide the keyboard by becoming first responder again.
  func summonKeyboard()
  
  /// Used internally and by API to hide the keyboard.
  func dismissKeyboard()
  
  /// Called to request display of the keyboard picker.
  /// Returns `false` if it cannot.
  func showKeyboardPicker() -> Bool
}
