/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-04-28
 *
 * DefaultsRepo is a protocol that exposes the ability to read, write and remove
 * Keyman UserDefaults settings.
 *
 */

import Foundation

public protocol DefaultsRepo {
  func readEnabledKeyboards() -> Set<String>
  func writeEnabledKeyboards(enabledKeyboardsArray: [String])
  func readSelectedKeyboard() -> String
  func writeSelectedKeyboard(keyboardName: String)
  func logDefaults()
  func clearDefaults()
}
