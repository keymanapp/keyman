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
      log.error("Error decoding keyboards: \(error)")
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
      log.error("Error encoding keyboards: \(error)")
    }
  }

  public func fullKeyboardID(forKey key: String) -> FullKeyboardID? {
    guard let data = data(forKey: key) else {
      return nil
    }
    do {
      return try PropertyListDecoder().decode(FullKeyboardID.self, from: data)
    } catch {
      log.error("Error decoding FullKeyboardID: \(error)")
      return nil
    }
  }

  public func set(_ fullKeyboardID: FullKeyboardID?, forKey key: String) {
    guard let id = fullKeyboardID else {
      removeObject(forKey: key)
      return
    }
    do {
      let data = try PropertyListEncoder().encode(id)
      set(data, forKey: key)
    } catch {
      log.error("Error encoding FullKeyboardID: \(error)")
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

  public var currentKeyboardID: FullKeyboardID? {
    get {
      return fullKeyboardID(forKey: Key.userCurrentKeyboard)
    }

    set(fullKeyboardID) {
      set(fullKeyboardID, forKey: Key.userCurrentKeyboard)
    }
  }

  public func userKeyboard(withFullID fullID: FullKeyboardID) -> InstallableKeyboard? {
    return userKeyboards?.first { $0.fullID == fullID }
  }

  var migrationLevel: Int {
    get {
      return integer(forKey: Key.migrationLevel)
    }

    set(level) {
      set(level, forKey: Key.migrationLevel)
    }
  }
}
