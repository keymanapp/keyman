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
    static let khmer_angkor =  InstallableKeyboard(id: "khmer_angkor",
                                                   name: "Khmer Angkor",
                                                   languageID: "km",
                                                   languageName: "Khmer (Central)",
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
  }
}
