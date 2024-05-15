//
//  InstallableLexicalModel.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

struct InstallableConstants {
  static let defaultVersion = "1.0"
}

/// Mainly differs from the API `LexicalModel` by having an associated language.
public struct InstallableLexicalModel: Codable, KMPInitializableLanguageResource {
  typealias Metadata = KMPLexicalModel
  public typealias Package = LexicalModelKeymanPackage
  
  // Details what properties are coded and decoded re: serialization.
  enum CodingKeys: String, CodingKey {
    case id
    case packageID
    case name
    case lgCode = "languageID"  // Redirects the old plain-property to something we can wrap with accessors.
    case version
    case isCustom
  }
  
  public private(set) var id: String
  public internal(set) var packageID: String? = nil
  public var name: String
  private var lgCode: String
  public var version: String
  public var isCustom: Bool
  
  public var languageID: String {
    return lgCode.lowercased()
  }
  
  public var sharableURL: String? {
    get {
      // We currently don't have any available sharing links for lexical models online.
      return nil
    }
  }
  
  // Weird scheme due to https://stackoverflow.com/a/58774558
  public var typedFullID: FullLexicalModelID {
    return FullLexicalModelID(lexicalModelID: id, languageID: languageID)
  }
  
  public var fullID: FullLexicalModelID {
    return typedFullID
  }
  
  public init(id: String,
              name: String,
              languageID: String,
              version: String,
              isCustom: Bool) {
    self.id = id
    self.name = name
    self.lgCode = languageID
    self.version = version
    self.isCustom = isCustom
  }
  
  public init(lexicalModel: LexicalModel, languageID: String, isCustom: Bool) {
    self.id = lexicalModel.id
    self.name = lexicalModel.name
    self.lgCode = languageID
    self.version = lexicalModel.version ?? InstallableConstants.defaultVersion
    self.isCustom = isCustom
  }
  
  internal init?(from metadata: KMPLexicalModel, packageID: String, lgCode: String) {
    self.id = metadata.id
    self.name = metadata.name
    self.lgCode = lgCode
    self.version = metadata.version!
    self.isCustom = false
    self.packageID = packageID
  }
  
  // Lexical models don't bundle fonts.  At least, not yet?
  public var fonts: [Font] {
    return []
  }
  
  public var sourceFilename: String {
    return "\(id).model.js"
  }
}

