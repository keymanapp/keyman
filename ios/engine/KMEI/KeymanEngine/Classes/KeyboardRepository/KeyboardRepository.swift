//
//  KeyboardRepository.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-12-01.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

public protocol KeyboardRepository: class {
  typealias CompletionHandler = (Error?) -> Void

  var delegate: KeyboardRepositoryDelegate? { get set }
  var languages: [String: Language]? { get }
  var keyboards: [String: Keyboard]? { get }

  func installableKeyboard(withID keyboardID: String, languageID: String) -> InstallableKeyboard?
  func fetch(completionHandler: CompletionHandler?)
}

public extension KeyboardRepository {
  public func installableKeyboard(withID keyboardID: String, languageID: String) -> InstallableKeyboard? {
    guard let keyboard = keyboards?.first(where: { $0.key == keyboardID })?.value else {
      return nil
    }
    
    // If the Keyboard (still) supports the requested language, use that one.
    guard let language = keyboard.languages?.first(where: {$0.id == languageID}) ??
        // Otherwise, just use the first language listed for the keyboard.
        keyboard.languages?.first ??
        // In cases where the keyboard fails to specify any language, use the requested one if it's in the collection.
        languages?[languageID]
    else {
        return nil
    }
    
    return InstallableKeyboard(keyboard: keyboard, language: language, isCustom: false)
  }

  public func fetch(completionHandler: CompletionHandler? = nil) {
    fetch(completionHandler: completionHandler)
  }
}
