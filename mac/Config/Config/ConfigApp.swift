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
  /*
   from AppDelegate
   [SentrySDK startWithConfigureOptions:^(SentryOptions *options) {
     options.dsn = @"https://960f8b8e574c46e3be385d60ce8e1fea@o1005580.ingest.sentry.io/5983522";
     options.releaseName = releaseName;
     options.environment = keymanVersionInfo.sentryEnvironment;
   }];

   */
    let sentryDsnUrl = "https://960f8b8e574c46e3be385d60ce8e1fea@o1005580.ingest.sentry.io/5983522"

    // Initialize Sentry only once here
    SentrySDK.start { options in
      options.dsn = sentryDsnUrl
      options.releaseName = ConfigAppUtil.versionGitTag
      options.environment = ConfigAppUtil.sentryEnvironment
      
      options.debug = true // Turn off in production
      options.tracesSampleRate = 1.0 // Adjust tracking rate for production
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
    
    // for testing purposes
//    Window("Install Test", id: "install-debug") {
//      InstallDebugView()
//        .environmentObject(installation)
//    }
  }
}
