//
//  KMPSystem.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

struct KMPSystem: Codable {
  public var keymanDeveloperVersion: String?
  public var fileVersion: String
  
  enum CodingKeys: String, CodingKey {
    case keymanDeveloperVersion
    case fileVersion
  }
  
  init?(forPackageType packageType: KMPMetadata.PackageType) {
    keymanDeveloperVersion = nil
    
    switch packageType {
    case .Keyboard:
      fileVersion = "7.0"
      return
    case .LexicalModel:
      fileVersion = "12.0"
      return
    default:
      return nil
    }
  }
}
