//
//  KeyboardCommandStructs.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 5/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation

struct Transform: Codable {
  var id: Int?
  var insert: String
  var deleteLeft: Int?
  var deleteRight: Int?
}

struct Suggestion: Codable {
  var transformId: Int
  var displayAs: String
  var transform: Transform
}

struct SuggestionPopup: Codable {
  var suggestion: Suggestion
  var x: Float
  var y: Float
  var width: Float
  var height: Float
  var isCustom: Bool
}
