//
//  LanguageResource.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/16/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public enum LanguageResourceType {
  case keyboard, lexicalModel
}

public protocol LanguageResourceFullID {
  var id: String { get }
  var languageID: String { get }
  var type: LanguageResourceType { get }
}

// Alas, 'associatedtype' stuff isn't exactly generic, and it's impossible to wildcard.
// So, this supports Swift's "type erasure" pattern, acting as a "wildcarded"
// LanguageResource that doesn't care about the specific associatedtype(s) that
// specific LanguageResources use.
public protocol AnyLanguageResource {
  var id: String { get }
  var languageID: String { get }
  var fullID: LanguageResourceFullID { get }
  var version: String { get }

  // Used for generating QR codes.
  var sharableURL: String? { get }

  // Used during resource installation
  var fonts: [Font] { get }
  var sourceFilename: String { get }
}

// Necessary due to Swift details 'documented' at
// https://stackoverflow.com/questions/42561685/why-cant-a-get-only-property-requirement-in-a-protocol-be-satisfied-by-a-proper
public protocol LanguageResource: AnyLanguageResource {
  associatedtype FullID: LanguageResourceFullID
  var typedFullID: FullID { get }
}

extension LanguageResource {
  // Thanks to https://stackoverflow.com/a/58774558 (on same thread mentioned above)
  // for this approach.
  public var fullID: LanguageResourceFullID {
    return typedFullID
  }
}
