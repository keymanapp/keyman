//
//  Packages.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/20/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
@testable import KeymanEngine

extension TestUtils {
  enum Packages {
    enum Keys {
      // For keyboards
      static let balochi_latin = KeymanPackage.Key(id: "balochi_latin", type: .keyboard)
      static let basic_kbdfr = KeymanPackage.Key(id: "basic_kbdfr", type: .keyboard)
      static let basic_kbdth0 = KeymanPackage.Key(id: "basic_kbdth0", type: .keyboard)
      static let fv_sencoten = KeymanPackage.Key(id: "fv_sencoten", type: .keyboard)
      static let indigenous_nt = KeymanPackage.Key(id: "indigenous_nt", type: .keyboard)
      static let khmer10 = KeymanPackage.Key(id: "khmer10", type: .keyboard)
      static let khmer_angkor = KeymanPackage.Key(id: "khmer_angkor", type: .keyboard)
      static let sil_cameroon_qwerty = KeymanPackage.Key(id: "sil_cameroon_qwerty", type: .keyboard)
      static let sil_euro_latin = KeymanPackage.Key(id: "sil_euro_latin", type: .keyboard)
      static let sil_ipa = KeymanPackage.Key(id: "sil_ipa", type: .keyboard)
      
      // For lexical models
      static let nrc_en_mtnt = KeymanPackage.Key(id: "nrc.en.mtnt", type: .lexicalModel)
      static let nrc_str_sencoten = KeymanPackage.Key(id: "nrc.str.sencoten", type: .lexicalModel)
      static let sil_bcc_latn_upp_ptwl1 = KeymanPackage.Key(id: "sil.bcc-latn-upp_ptwl1", type: .lexicalModel)
    }
    
    static func getURLForPackage(withKey key: KeymanPackage.Key) -> URL? {
      switch key.type {
      case .keyboard:
        return TestUtils.keyboardsBundle.url(forResource: key.id, withExtension: "kmp")
      case .lexicalModel:
        return TestUtils.lexicalModelsBundle.url(forResource: "\(key.id).model", withExtension: "kmp")
      }
    }
  }
}
