//
//  Keyboards.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/19/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
@testable import KeymanEngine

extension TestUtils {
  enum Keyboards {
    static let khmerAngkorKMP = TestUtils.keyboardsBundle.url(forResource: "khmer_angkor", withExtension: "kmp")!
    static let silEuroLatinKMP = TestUtils.keyboardsBundle.url(forResource: "sil_euro_latin", withExtension: "kmp")!
    static let fvSencotenKMP = TestUtils.keyboardsBundle.url(forResource: "fv_sencoten", withExtension: "kmp")!
    
    static let khmer_angkor =  InstallableKeyboard(id: "khmer_angkor",
                                                   name: "Khmer Angkor",
                                                   languageID: "km",
                                                   languageName: "Central Khmer (Khmer, Cambodia)",
                                                   version: "1.0.6",
                                                   isRTL: false,
                                                   font: nil,
                                                   oskFont: nil,
                                                   isCustom: false)
    
    static let khmer10 =  InstallableKeyboard(id: "khmer10",
                                              name: "Khmer (NiDA)",
                                              languageID: "km",
                                              languageName: "Khmer (Central)",
                                              version: "1.3",
                                              isRTL: false,
                                              font: nil,
                                              oskFont: nil,
                                              isCustom: false)
    
    static let sil_euro_latin = InstallableKeyboard(id: "sil_euro_latin",
                                                    name: "EuroLatin (SIL)",
                                                    languageID: "en",
                                                    languageName: "English",
                                                    version: "1.9.1",
                                                    isRTL: false,
                                                    font: Font(family: "LatinWeb", source: ["DejaVuSans.ttf"], size: nil),
                                                    oskFont: nil,
                                                    isCustom: false)
    
    static let fv_sencoten = InstallableKeyboard(id: "fv_sencoten",
                                                 name: "SENĆOŦEN",
                                                 languageID: "str-latn",
                                                 languageName: "Salish, Straits (Latin)",
                                                 version: "9.1",
                                                 isRTL: false,
                                                 font: nil,
                                                 oskFont: nil,
                                                 isCustom: false)
  }
}
