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

func trimDirectionalMarkPrefix(_ str: String?) -> String {
  var text = str ?? ""
  
  let DIRECTIONAL_MARKS = [
    UnicodeScalar("\u{200e}"),
    UnicodeScalar("\u{200f}"),
    UnicodeScalar("\u{202e}")
  ]
  
  // If the first intended char in context is a diacritic (such as U+0300), Swift's
  // standard string-handling will treat it and a preceding directional character
  // as a single unit.  This code block exists to avoid the issue.
  if text.unicodeScalars.count > 0 {
    let head = text.unicodeScalars.first!
    let tail = String(text.unicodeScalars.dropFirst(1))
    
    //    // "%02X" - to hex-format the integer for the string conversion.
    //    let headEncoding = String(format:"%02X", text.utf16[text.utf16.startIndex])
    //    os_log("head: '%@', tail: '%@'", log: KeymanEngineLogger.engine, type: .debug, headEncoding, tail)
    
    // Remove any system-added LTR/RTL marks.
    if DIRECTIONAL_MARKS.contains(where: { head.value == $0.value }) {
      text = tail
    }
  }
  return text
}
