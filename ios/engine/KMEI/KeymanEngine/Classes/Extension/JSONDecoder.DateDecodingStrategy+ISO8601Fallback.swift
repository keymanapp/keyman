//
//  JSONDecoder.DateDecodingStrategy+ISO8601Fallback.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-16.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

extension JSONDecoder.DateDecodingStrategy {
  static var ios8601WithFallback: JSONDecoder.DateDecodingStrategy {
    if #available(iOS 10.0, *) {
      return .iso8601
    }
    let formatter = DateFormatter()
    formatter.locale = Locale(identifier: "en_US_POSIX")
    formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ssZZZZZ"
    return .formatted(formatter)
  }
}
