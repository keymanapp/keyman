//
//  KeymanWebDelegate.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-17.
//  Copyright © 2017 SIL International. All rights reserved.
//

import WebKit

/// Delegate receiving events from Keyman Web.
protocol KeymanWebDelegate: class {
  /// Keyman Web has loaded (or reloaded).
  func keyboardLoaded(_ keymanWeb: KeymanWebViewController)
  
  /// - Parameters:
  ///   - numCharsToDelete: The number of UTF-16 code units to delete before inserting the new text.
  ///   - newText: The string to insert.
  func insertText(_ keymanWeb: KeymanWebViewController, numCharsToDelete: Int, newText: String)
  
  /// - Parameters:
  func beep(_ keymanWeb: KeymanWebViewController)
  
  /// - Parameters:
  ///   - keyFrame: The frame of the anchor key.
  ///   - preview: The text to preview.
  func showKeyPreview(_ keymanWeb: KeymanWebViewController, keyFrame: CGRect, preview: String)
  
  func dismissKeyPreview(_ keymanWeb: KeymanWebViewController)
  
  /// - Parameters:
  ///   - keyFrame: The frame of the anchor key.
  ///   - subkeyIDs: The IDs of the subkeys.
  ///   - subkeyTexts: The user-displayable texts of the subkeys.
  ///   - useSpecialFont: Use the Keyman Web OSK font.
  func showSubkeys(_ keymanWeb: KeymanWebViewController,
                   keyFrame: CGRect,
                   subkeyIDs: [String],
                   subkeyTexts: [String],
                   useSpecialFont: Bool)
  func menuKeyDown(_ keymanWeb: KeymanWebViewController)
  func menuKeyUp(_ keymanWeb: KeymanWebViewController)
  func menuKeyHeld(_ keymanWeb: KeymanWebViewController)
}

extension KeymanWebDelegate {
  func keyboardLoaded(_ keymanWeb: KeymanWebViewController) {}
  func insertText(_ keymanWeb: KeymanWebViewController, numCharsToDelete: Int, newText: String) {}
  func beep(_ keymanWeb: KeymanWebViewController) {}
  func showKeyPreview(_ keymanWeb: KeymanWebViewController, keyFrame: CGRect, preview: String) {}
  func dismissKeyPreview(_ keymanWeb: KeymanWebViewController) {}
  func showSubkeys(_ keymanWeb: KeymanWebViewController,
                   keyFrame: CGRect,
                   subkeyIDs: [String],
                   subkeyTexts: [String],
                   useSpecialFont: Bool) {}
  func menuKeyDown(_ keymanWeb: KeymanWebViewController) {}
  func menuKeyUp(_ keymanWeb: KeymanWebViewController) {}
  func menuKeyHeld(_ keymanWeb: KeymanWebViewController) {}
}
