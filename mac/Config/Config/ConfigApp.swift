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
    
    
    Window("Configuration", id: "config") {
      ConfigView()
        .environmentObject(settings)
        .task {
          if !installation.isInstallationComplete() {
            openWindow(id: "install")
            openWindow(id: "new install")
          }
        }
    }
    Window("Installation", id: "install") {
      InstallView()
        .environmentObject(installation)
    }
    
    
    Window("New Installation", id: "new install") {
      ParentInstallView()
        .environmentObject(installation)
    }
    .defaultSize(width: 500, height: 400)
    .windowResizability(.contentSize)
  
  }
}
