//
//  KeyboardError.swift
//  KeymanEngine
//
//  Created by Jacob Bullock on 5/1/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation

enum KeyboardError: String, Error {
  case fileMissing = "error-missing-file"
  case keyboardLoadingError = "error-loading-keyboard"
  case lexicalModelLoadingError = "error-loading-lexical-model"
}
