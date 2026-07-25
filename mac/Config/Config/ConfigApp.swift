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
    .commands {
      CommandGroup(replacing: .appInfo) {
        Button {
          AboutPanelPresenter.showAboutPanel()
        } label: {
          Label("About Keyman Configuration", systemImage: "info.circle")
        }
      }
    }
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
