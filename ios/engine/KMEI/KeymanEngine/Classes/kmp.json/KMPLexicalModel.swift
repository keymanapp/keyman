//
//  KMPLexicalModel.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/12/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

public class KMPLexicalModel: Codable {
  public var name: String
  public var lexicalModelId: String
  public var version: String?
  public var isRTL: Bool = false
  public var languages: [KMPLanguage]
  public var installableLexicalModels: [InstallableLexicalModel]! = []

  enum CodingKeys: String, CodingKey {
    case name
    case lexicalModelId = "id"
    case version
    case isRTL = "rtl"
    case languages
  }

  required public init(from decoder: Decoder) throws {
    let values = try decoder.container(keyedBy: CodingKeys.self)

    name = try values.decode(String.self, forKey: .name)
    lexicalModelId = try values.decode(String.self, forKey: .lexicalModelId)
    // Our schema says that this field is optional for lexical models.
    // In these cases, the version should probably be set to the package's version...
    // but this constructor lacks access.  We'll let the owning Package instance handle this.
    version = try values.decodeIfPresent(String.self, forKey: .version)
    isRTL = try values.decodeIfPresent(Bool.self, forKey: .isRTL) ?? false
    languages = try values.decode([KMPLanguage].self, forKey: .languages)

    // Now to build the InstallableKeyboard list.
    var installableLexicalModels : [InstallableLexicalModel] = []

    for language in self.languages {
      let model = InstallableLexicalModel(id: lexicalModelId, name: name,
                                              languageID: language.languageId,
                                              version: version ?? "",
                                              isCustom: true) //update this based on adhoc vs api

      installableLexicalModels.append( model )
    }

    self.installableLexicalModels = installableLexicalModels
  }

  public var isValid: Bool {
    // Ensures that our 'optional' members are properly instantiated.
    // Any file-based checks will be performed by the object's owner,
    // which knows the containing package's root folder.
    return installableLexicalModels.count > 0
      && self.languages.count > 0
  }
}

