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
  var languages: [KMPLanguage] { get set } // set is for adding languages during 13.0 -> 14.0 migration
  var installableResources: [AnyLanguageResource] { get }
}

protocol KMPResource: AnyKMPResource {
  associatedtype LanguageResourceType: KMPInitializableLanguageResource where LanguageResourceType.Metadata == Self
  
  // Used to convert old cloud resources into the extracted KMP format for 14.0+ file management.
  init?(from resource: LanguageResourceType)
  
  var typedInstallableResources: [LanguageResourceType] { get }
  
  /**
   * Designed to facilitate de-duplication of KMPResources when migrating cloud resources into the extracted KMP format
   * for 14.0+ file management.
   */
  func hasMatchingMetadata(for resource: LanguageResourceType, ignoreLanguage: Bool, ignoreVersion: Bool) -> Bool
}

extension KMPResource {
  var installableResources: [AnyLanguageResource] {
    return typedInstallableResources
  }
}
