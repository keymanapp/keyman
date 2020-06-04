//
//  KMPResource.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/3/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

internal protocol KMPResource {
  // Returns the represented resource's ID.
  var id: String { get }

  var installableResources: [LanguageResource] { get }

  // Used to convert old cloud resources into the extracted KMP format for 14.0+ file management.
  init?(from resource: LanguageResource)

  func matches(installable resource: LanguageResource, requireLanguageMatch: Bool) -> Bool
}
