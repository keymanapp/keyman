/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * The Configuration Application App object
 */

import SwiftUI
import AppKit
import KeymanSettings
import OSLog
import Sentry

extension Logger {
  private static let configSubsystem = ConfigAppUtil.configBundleId
  static let app = Logger(subsystem: configSubsystem, category: "app")
  static let download   = Logger(subsystem: configSubsystem, category: "download")
}

@main
struct ConfigApp: App {
  @StateObject var settings = SettingsContainer()
  @StateObject var installation = InstallationContainer()
  @Environment(\.openWindow) private var openWindow
  
  init() {
    Logger.app.log("Starting Keyman Configuration, version: \(ConfigAppUtil.versionWithTag), versionWithTag: \(ConfigAppUtil.versionWithTag)")
    let sentryDsnUrl = "https://960f8b8e574c46e3be385d60ce8e1fea@o1005580.ingest.sentry.io/5983522"

    // Initialize Sentry only once here
    SentrySDK.start { options in
      options.dsn = sentryDsnUrl
      options.releaseName = ConfigAppUtil.versionGitTag
      options.environment = ConfigAppUtil.sentryEnvironment
    }
  }

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
    .commands {
      CommandGroup(replacing: .appInfo) {
        Button {
          AboutPanelPresenter.showAboutPanel()
        } label: {
          Label("About Keyman Configuration", systemImage: "info.circle")
        }
      }
    }
    
    // for testing purposes
//    Window("Install Test", id: "install-debug") {
//      InstallDebugView()
//        .environmentObject(installation)
//    }
  }
}

@MainActor
private enum AboutPanelPresenter {
  private static var aboutWindow: NSWindow?
  
  static func showAboutPanel() {
    let contentView = AboutPanelView()
    
    let window = aboutWindow ?? makeAboutWindow()
    window.contentView = NSHostingView(rootView: contentView)
    window.center()
    window.makeKeyAndOrderFront(nil)
    aboutWindow = window
    
    NSApp.activate(ignoringOtherApps: true)
  }
  
  private static func makeAboutWindow() -> NSWindow {
    let window = NSWindow(
      contentRect: NSRect(x: 0, y: 0, width: 570, height: 200),
      styleMask: [.titled, .closable],
      backing: .buffered,
      defer: false
    )
    
    window.titleVisibility = .hidden
    window.titlebarAppearsTransparent = true
    window.isReleasedWhenClosed = false
    window.backgroundColor = .windowBackgroundColor
    return window
  }
}
