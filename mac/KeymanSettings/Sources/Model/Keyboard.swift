/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Wrapper bject that represents a Keyman keyboard as loaded from a .KMP file
 * Can be loaded but disabled
 *
 */


import Foundation
import AppKit

public struct Keyboard: Identifiable, Hashable, Equatable {
  
  public var id = UUID()
  public var enabled: Bool
  public var name: String
  public var keyboardId: String
  
  public init(keyboardSource: KeyboardSource) {
    self.enabled = true
    self.name = keyboardSource.name
    self.keyboardId = keyboardSource.id
    
//    ConfigLogger.shared.testLogger.debug("keyboard created for: \(keyboardSource.id)")
    print("keyboard created for: \(keyboardSource.id)")
  }
}
