//
//  PackageJSON.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

extension TestUtils {
  enum PackageJSON {
    static let jsonBundle = findSubBundle(forResource: "PackageJSON", ofType: "bundle")
    
    // place entries here.
    static let language_en = jsonBundle.url(forResource: "language-en", withExtension: "json")!
    static let language_km = jsonBundle.url(forResource: "language-km", withExtension: "json")!
    static let language_str_latn = jsonBundle.url(forResource: "language-str-Latn", withExtension: "json")!
    
    static let keyboard_khmer_angkor = jsonBundle.url(forResource: "keyboard-khmer_angkor", withExtension: "json")!
    
    static let model_nrc_en_mtnt = jsonBundle.url(forResource: "model-nrc.en.mtnt", withExtension: "json")!
    
    static let kmp_json_khmer_angkor = jsonBundle.url(forResource: "kmp-json-khmer_angkor", withExtension: "json")!
    static let kmp_json_nrc_en_mtnt = jsonBundle.url(forResource: "kmp-json-nrc.en.mtnt", withExtension: "json")!
  }
}
