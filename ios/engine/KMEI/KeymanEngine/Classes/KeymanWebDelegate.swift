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
  func keyboardLoaded(_ view: KeymanWebViewController)

  /// - Parameters:
  ///   - numCharsToDelete: The number of UTF-16 code units to delete before inserting the new text.
  ///   - newText: The string to insert.
  func insertText(_ view: KeymanWebViewController, numCharsToDelete: Int, newText: String)

  /// - Parameters:
  ///   - keyFrame: The frame of the anchor key.
  ///   - preview: The text to preview.
  func showKeyPreview(_ view: KeymanWebViewController, keyFrame: CGRect, preview: String)

  func dismissKeyPreview(_ view: KeymanWebViewController)

  /// - Parameters:
  ///   - keyFrame: The frame of the anchor key.
  ///   - subkeyIDs: The IDs of the subkeys.
  ///   - subkeyTexts: The user-displayable texts of the subkeys.
  ///   - useSpecialFont: Use the Keyman Web OSK font.
  func showSubkeys(_ view: KeymanWebViewController,
                   keyFrame: CGRect,
                   subkeyIDs: [String],
                   subkeyTexts: [String],
                   useSpecialFont: Bool)
  func menuKeyDown(_ view: KeymanWebViewController)
  func menuKeyUp(_ view: KeymanWebViewController)
  func menuKeyHeld(_ view: KeymanWebViewController)
  func hideKeyboard(_ view: KeymanWebViewController)
}

extension KeymanWebDelegate {
  func keyboardLoaded(_ view: KeymanWebViewController) {}
  func insertText(_ view: KeymanWebViewController, numCharsToDelete: Int, newText: String) {}
  func showKeyPreview(_ view: KeymanWebViewController, keyFrame: CGRect, preview: String) {}
  func dismissKeyPreview(_ view: KeymanWebViewController) {}
  func showSubkeys(_ view: KeymanWebViewController,
                   keyFrame: CGRect,
                   subkeyIDs: [String],
                   subkeyTexts: [String],
                   useSpecialFont: Bool) {}
  func menuKeyDown(_ view: KeymanWebViewController) {}
  func menuKeyUp(_ view: KeymanWebViewController) {}
  func menuKeyHeld(_ view: KeymanWebViewController) {}
  func hideKeyboard(_ view: KeymanWebViewController) {}
}
