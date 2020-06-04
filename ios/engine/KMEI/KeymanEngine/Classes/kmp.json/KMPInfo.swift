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
  var name: KMPInfoItem? = nil
  var author: KMPInfoItem? = nil
  var copyright: KMPInfoItem? = nil
  var website: KMPInfoItem? = nil
  // If we're constructing one from scratch, set to the absolute minimal version.
  // This will automatically signal that the 'package' needs to be updated.
  var version: KMPInfoItem? = KMPInfoItem(description: "0.0.0")

  enum CodingKeys: String, CodingKey {
    case name
    case author
    case copyright
    case website
    case version
  }
}
