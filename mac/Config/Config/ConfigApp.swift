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
  @StateObject var settings = SettingsContainer()
  @StateObject var installation = InstallationContainer()
  @Environment(\.openWindow) private var openWindow
  
  var body: some Scene {
    Window("Config Test", id: "config-debug") {
      ConfigDebugView()
        .environmentObject(settings)
        .task {
          if !installation.getHasDisplayedInstallationComplete() {
            openWindow(id: "install")
          }
        }
        .onReceive(NotificationCenter.default.publisher(for: .installationRepairStarted)) { notification in openWindow(id: "install")
        }
    }
    Window("Install Test", id: "install-debug") {
      InstallDebugView()
        .environmentObject(installation)
    }
    Window("Installation", id: "install") {
      MainInstallView()
        .environmentObject(installation)
    }
    .windowResizability(.contentSize)
    .defaultSize(width: 600, height: 450)
    Window("Main Configuration", id: "main_config") {
      MainConfigView()
        .environmentObject(settings)
    }
  }
}
