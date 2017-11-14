//
//  UserDefaults+Types.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-26.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

public extension UserDefaults {
  public func installableKeyboards(forKey key: String) -> [InstallableKeyboard]? {
    guard let array = array(forKey: key) as? [Data] else {
      return nil
    }
    let decoder = PropertyListDecoder()
    do {
      return try array.map { try decoder.decode(InstallableKeyboard.self, from: $0) }
    } catch {
      Manager.shared.kmLog("UserDefaults: Error decoding keyboards: \(error)", checkDebugPrinting: false)
      return nil
    }
  }

  public func set(_ keyboards: [InstallableKeyboard]?, forKey key: String) {
    guard let keyboards = keyboards else {
      removeObject(forKey: key)
      return
    }
    let encoder = PropertyListEncoder()
    do {
      let array = try keyboards.map { try encoder.encode($0) }
      set(array, forKey: key)
    } catch {
      Manager.shared.kmLog("UserDefaults: Error encoding keyboards: \(error)", checkDebugPrinting: false)
    }
  }

  public func installableKeyboard(forKey key: String) -> InstallableKeyboard? {
    guard let data = data(forKey: key) else {
      return nil
    }
    do {
      return try PropertyListDecoder().decode(InstallableKeyboard.self, from: data)
    } catch {
      Manager.shared.kmLog("UserDefaults: Error decoding keyboard: \(error)", checkDebugPrinting: false)
      return nil
    }
  }

  public func set(_ keyboard: InstallableKeyboard?, forKey key: String) {
    guard let keyboard = keyboard else {
      removeObject(forKey: key)
      return
    }
    do {
      let data = try PropertyListEncoder().encode(keyboard)
      set(data, forKey: key)
    } catch {
      Manager.shared.kmLog("UserDefaults: Error encoding keyboard: \(error)", checkDebugPrinting: false)
    }
  }

  public var userKeyboards: [InstallableKeyboard]? {
    get {
      return installableKeyboards(forKey: Key.userKeyboardsList)
    }

    set(keyboards) {
      set(keyboards, forKey: Key.userKeyboardsList)
    }
  }

  public var currentKeyboard: InstallableKeyboard? {
    get {
      return installableKeyboard(forKey: Key.userCurrentKeyboard)
    }

    set(keyboard) {
      set(keyboard, forKey: Key.userCurrentKeyboard)
    }
  }

  public func userKeyboard(withID keyboardID: String, languageID: String) -> InstallableKeyboard? {
    return userKeyboards?.first { $0.id == keyboardID && $0.languageID == languageID }
  }
}
