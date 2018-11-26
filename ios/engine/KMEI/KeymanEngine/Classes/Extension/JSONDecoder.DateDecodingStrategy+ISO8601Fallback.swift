//
//  JSONDecoder.DateDecodingStrategy+ISO8601Fallback.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-16.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

extension JSONDecoder.DateDecodingStrategy {
  static var iso8601WithoutTimezone: JSONDecoder.DateDecodingStrategy {
    let formatter = DateFormatter()
    formatter.locale = Locale(identifier: "en_US_POSIX")
    formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ss"
    return .formatted(formatter)
  }
  static var ios8601WithFallback: JSONDecoder.DateDecodingStrategy {
    if #available(iOS 10.0, *) {
      return .iso8601
    }
    let formatter = DateFormatter()
    formatter.locale = Locale(identifier: "en_US_POSIX")
    formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ssZZZZZ"
    return .formatted(formatter)
  }
  
  static var ios8601WithMilliseconds: JSONDecoder.DateDecodingStrategy {
    let formatter = DateFormatter()
    formatter.calendar = Calendar(identifier: .iso8601)
    formatter.locale = Locale(identifier: "en_US_POSIX")
    formatter.timeZone = TimeZone(secondsFromGMT: 0)
    formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX"
    return .formatted( formatter )
  }
}
