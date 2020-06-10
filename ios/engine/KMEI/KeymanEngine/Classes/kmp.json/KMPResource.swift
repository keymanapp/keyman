//
//  KMPResource.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/3/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

internal protocol AnyKMPResource {
  // Returns the represented resource's ID.
  var id: String { get }

  var installableResources: [AnyLanguageResource] { get }

  // Used to convert old cloud resources into the extracted KMP format for 14.0+ file management.
  init?(from resource: AnyLanguageResource)

  func matches(installable resource: AnyLanguageResource, requireLanguageMatch: Bool) -> Bool
}

protocol KMPResource: AnyKMPResource {
  associatedtype LanguageResourceType: LanguageResource
  var typedInstallableResources: [LanguageResourceType] { get }
}

extension KMPResource {
  var installableResources: [AnyLanguageResource] {
    return typedInstallableResources
  }
}
