//
//  String+Helpers.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-12.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

extension String {
  var hasFontExtension: Bool {
    return hasExtension(FileExtensions.trueTypeFont) || hasSuffix(FileExtensions.openTypeFont)
  }

  var hasJavaScriptExtension: Bool {
    return hasExtension(FileExtensions.javaScript)
  }

  func hasExtension(_ ext: String) -> Bool {
    return lowercased().hasSuffix(".\(ext)")
  }

  /// - Parameter separator: The separator between code units.
  /// - Returns: This String interpreted as hex-encoded UTF-16 code units.
  func stringFromUTF16CodeUnits(separatedBy separator: String = ",") -> String? {
    // TODO: Use one scanner for the whole String instead of splitting by comma first
    let hexStrings = components(separatedBy: separator)

    var codeUnits: [UInt16] = []
    for hexString in hexStrings {
      let scanner = Scanner(string: hexString)
      var codeUnit: UInt32 = 0
      if scanner.scanHexInt32(&codeUnit) && scanner.isAtEnd {
        codeUnits.append(UInt16(codeUnit))
      } else {
        return nil
      }
    }
    return String(utf16CodeUnits: codeUnits, count: codeUnits.count)
  }
}
