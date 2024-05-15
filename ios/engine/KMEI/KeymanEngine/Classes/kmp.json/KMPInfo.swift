//
//  KMPInfo.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/2/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

class KMPInfoItem: Codable {
  var description: String
  var url: String?
  
  init(description: String, url: String? = nil) {
    self.description = description
    self.url = url
  }
}

class KMPInfo: Codable {
  static let DEFAULT_VERSION = KMPInfoItem(description: "1.0.0")
  static let AUTOGEN_VERSION = KMPInfoItem(description: "0.0.0")
  
  var name: KMPInfoItem? = nil
  var author: KMPInfoItem? = nil
  var copyright: KMPInfoItem? = nil
  var website: KMPInfoItem? = nil
  // If a package doesn't have this set, we default to 1.0.0.
  var version: KMPInfoItem? = KMPInfo.DEFAULT_VERSION
  
  enum CodingKeys: String, CodingKey {
    case name
    case author
    case copyright
    case website
    case version
  }
  
  convenience init(version: String) {
    self.init(version: KMPInfoItem(description: version))
  }
  
  init(version: KMPInfoItem) {
    self.version = version
  }
}
