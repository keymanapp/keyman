//
//  LexicalModels.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/19/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
@testable import KeymanEngine

extension TestUtils {
  enum LexicalModels {
    static let mtntKMP = TestUtils.lexicalModelsBundle.url(forResource: "nrc.en.mtnt.model", withExtension: "kmp")!
    static let mtnt = InstallableLexicalModel(id: "nrc.en.mtnt",
                                              name: "English dictionary (MTNT)",
                                              languageID: "en",
                                              version: "0.1.4",
                                              isCustom: false)
    
    static let sencotenKMP = TestUtils.lexicalModelsBundle.url(forResource: "nrc.str.sencoten.model",
                                                               withExtension: "kmp")!
    static let sencoten = InstallableLexicalModel(id: "nrc.str.sencoten",
                                                  name: "SENĆOŦEN (Saanich Dialect) Lexical Model",
                                                  languageID: "str",
                                                  version: "1.0.5",
                                                  isCustom: false)
  }
}
