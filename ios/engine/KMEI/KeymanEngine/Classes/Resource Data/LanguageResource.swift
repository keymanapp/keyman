//
//  LanguageResource.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/16/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public enum LanguageResourceType: String, Codable {
  case keyboard
  case lexicalModel
}

// Subclasses must implement Equatable; Swift doesn't like it directly on the root protocol.
public protocol AnyLanguageResourceFullID {
  var id: String { get }
  var languageID: String { get }
  var type: LanguageResourceType { get }
  
  func matches(_ other: AnyLanguageResourceFullID, requireLanguageMatch: Bool) -> Bool
}

extension AnyLanguageResourceFullID {
  // For matching when we're not sure if the underlying types actually match.
  public func matches(_ other: AnyLanguageResourceFullID, requireLanguageMatch: Bool = true) -> Bool {
    if requireLanguageMatch {
      return id == other.id && type == other.type && languageID == other.languageID
    } else {
      return id == other.id && type == other.type
    }
  }
}

// Equatable only really makes sense when the types are identical.
extension AnyLanguageResourceFullID where Self: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    return lhs.id == rhs.id && lhs.languageID == rhs.languageID && lhs.type == rhs.type
  }
}

extension AnyLanguageResourceFullID {
  var description: String {
    return "{\(type): {id = \(id), languageID=\(languageID)}}"
  }
}

public protocol LanguageResourceFullID: AnyLanguageResourceFullID {
  associatedtype Resource: LanguageResource where Resource.FullID == Self
}

// Alas, 'associatedtype' stuff isn't exactly generic, and it's impossible to wildcard.
// So, this supports Swift's "type erasure" pattern, acting as a "wildcarded"
// LanguageResource that doesn't care about the specific associatedtype(s) that
// specific LanguageResources use.
public protocol AnyLanguageResource {
  var id: String { get }
  var name: String { get }
  var languageID: String { get }
  // Was not always tracked within KeymanEngine - is optional for legacy reasons.
  var packageID: String? { get }
  var packageKey: KeymanPackage.Key { get }
  var fullID: AnyLanguageResourceFullID { get }
  var version: String { get }
  
  // Used for generating QR codes.
  var sharableURL: String? { get }
  
  // Used during resource installation
  var fonts: [Font] { get }
  var sourceFilename: String { get }
}

extension AnyLanguageResource {
  public var packageKey: KeymanPackage.Key {
    return KeymanPackage.Key(forResource: self)
  }
}

// Necessary due to Swift details 'documented' at
// https://stackoverflow.com/questions/42561685/why-cant-a-get-only-property-requirement-in-a-protocol-be-satisfied-by-a-proper
public protocol LanguageResource: AnyLanguageResource {
  associatedtype FullID: LanguageResourceFullID where FullID: Equatable, FullID.Resource == Self
  associatedtype Package: KeymanPackage
  var typedFullID: FullID { get }
}

extension LanguageResource {
  // Thanks to https://stackoverflow.com/a/58774558 (on same thread mentioned above)
  // for this approach.
  public var fullID: AnyLanguageResourceFullID {
    return typedFullID as AnyLanguageResourceFullID
  }
}

internal protocol KMPInitializableLanguageResource: LanguageResource {
  associatedtype Metadata: KMPResource where Metadata.LanguageResourceType == Self
  init?(from metadata: Metadata, packageID: String, lgCode: String)
}
