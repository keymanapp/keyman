/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * The Configuration Application App object
 */

import SwiftUI
import KeymanSettings
import OSLog

extension Logger {
  private static var subsystem = ConfigAppUtil.configBundleId
  static let package = Logger(subsystem: subsystem, category: "package")
  static let download   = Logger(subsystem: subsystem, category: "download")
  static let ui   = Logger(subsystem: subsystem, category: "ui")
}

@main
struct ConfigApp: App {
  @StateObject var settings = SettingsContainer()
  @StateObject var installation = InstallationContainer()
  @Environment(\.openWindow) private var openWindow
  
  var body: some Scene {
    Window("Configuration", id: "main-config") {
      MainConfigView()
        .frame(
            minWidth: 600, maxWidth: 1000,
            minHeight: 400, maxHeight: .infinity
        )
        .environmentObject(settings)
        .task {
          if !installation.getHasDisplayedInstallationComplete() {
            openWindow(id: "install")
          }
        }
        .onReceive(NotificationCenter.default.publisher(for: .installationRepairStarted)) { notification in openWindow(id: "install")
        }
    }
    // the size of the window when first opened
        .defaultSize(width: 800, height: 600)
        .windowResizability(.contentSize)
    
    Window("Installation", id: "install") {
      MainInstallView()
        .environmentObject(installation)
    }
    .windowResizability(.contentSize)
    .defaultSize(width: 600, height: 500)
    
    // for testing purposes
//    Window("Install Test", id: "install-debug") {
//      InstallDebugView()
//        .environmentObject(installation)
//    }
  }
}
