//
//  KMPResource.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 6/3/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

protocol AnyKMPResource {
  // Returns the represented resource's ID.
  var id: String { get }

  var installableResources: [AnyLanguageResource] { get }
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
