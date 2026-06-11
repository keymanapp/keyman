/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * The Configuration Application App object
 */

import SwiftUI
import KeymanSettings

@main
struct ConfigApp: App {
//  @StateObject var configuration = Configuration()
  @StateObject var settings = SettingsContainer()
  var body: some Scene {
    WindowGroup {
      ConfigView()
        .environmentObject(settings)
   }
  }
}
