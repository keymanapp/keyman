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

  weak var delegate: KeyboardRepositoryDelegate? { get set }
  var languages: [String: Language]? { get }

  func installableKeyboard(withID keyboardID: String, languageID: String) -> InstallableKeyboard?
  func fetch(completionHandler: CompletionHandler?)
}

public extension KeyboardRepository {
  public func installableKeyboard(withID keyboardID: String, languageID: String) -> InstallableKeyboard? {
    guard let language = languages?[languageID] else {
      return nil
    }
    guard let keyboard = language.keyboards?.first(where: { $0.id == keyboardID }) else {
      return nil
    }
    return InstallableKeyboard(keyboard: keyboard, language: language)
  }

  public func fetch(completionHandler: CompletionHandler? = nil) {
    fetch(completionHandler: completionHandler)
  }
}
