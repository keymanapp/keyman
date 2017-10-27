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

  public func set(_ keyboards: [InstallableKeyboard], forKey key: String) {
    let encoder = PropertyListEncoder()
    do {
      let array = try keyboards.map { try encoder.encode($0) }
      set(array, forKey: key)
    } catch {
      Manager.shared.kmLog("UserDefaults: Error encoding keyboards: \(error)", checkDebugPrinting: false)
    }
  }

  public func installableKeyboard(forKey key: String) -> InstallableKeyboard? {
    if let data = self.data(forKey: key) {
      do {
        return try PropertyListDecoder().decode(InstallableKeyboard.self, from: data)
      } catch {
        Manager.shared.kmLog("UserDefaults: Error decoding keyboard: \(error)", checkDebugPrinting: false)
        return nil
      }
    }
    return nil
  }

  public func set(_ keyboard: InstallableKeyboard, forKey key: String) {
    do {
      let data = try PropertyListEncoder().encode(keyboard)
      set(data, forKey: key)
    } catch {
      Manager.shared.kmLog("UserDefaults: Error encoding keyboard: \(error)", checkDebugPrinting: false)
    }
  }
}
