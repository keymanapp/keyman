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
}

class KMPInfo: Codable {
  var name: KMPInfoItem?
  var author: KMPInfoItem?
  var copyright: KMPInfoItem?
  var website: KMPInfoItem?
  var version: KMPInfoItem?

  enum CodingKeys: String, CodingKey {
    case name
    case author
    case copyright
    case website
    case version
  }
}
