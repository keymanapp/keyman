//
//  FullLexicalModelID.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

/// A complete identifier for an `InstallableLexicalModel`. LexicalModels must have unique `FullLexicalModelID`s.
public struct FullLexicalModelID: Codable, LanguageResourceFullID, Equatable {
  public typealias Resource = InstallableLexicalModel
  
  public var lexicalModelID: String
  public var languageID: String
  
  public var id: String {
    return lexicalModelID
  }
  
  public var type: LanguageResourceType {
    return .lexicalModel
  }
}

// MARK: - CustomStringConvertible
extension FullLexicalModelID: CustomStringConvertible {
  public var description: String {
    return "<lexicalModelID: \"\(lexicalModelID)\", languageID: \"\(languageID)\">"
  }
}

