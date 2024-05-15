//
//  RegisteredFont.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-20.
//  Copyright © 2017 SIL International. All rights reserved.
//

public struct RegisteredFont {
  public let name: String
  public var isRegistered: Bool
  
  public init(name: String, isRegistered: Bool) {
    self.name = name
    self.isRegistered = isRegistered
  }
}
