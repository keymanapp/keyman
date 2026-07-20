/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * Main view used for configuring Keyman
 */

import SwiftUI

struct InstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  
  var body: some View {
    VStack {
      HStack {
        Image(systemName: "gear")
          .imageScale(.large)
          .foregroundColor(.accentColor)
        if let nextTask = installation.nextTask() {
          Text("Next task = \(nextTask.taskType.rawValue)")
        }
      }
      HStack {
        Button("Next...") {
          installation.executeNextInstallationTask()
        }
        .disabled(installation.isInstallationComplete())
        Button("Migrate Data") {
          _ = installation.migrateData()
        }
        Button("Register Keyman") {
          _ = installation.registerKeymanInputMethod()
        }
        Button("Enable Keyman") {
          _ = installation.enableKeymanInputMethod()
        }
        Button("Select Keyman") {
          _ = installation.selectKeymanInputMethod()
        }
        Button("Check Permission") {
          installation.checkAccessibilityPermissionGranted()
        }
        Button("Request Permission") {
          _ = installation.requestAccessibility()
        }
        Spacer()
      }
      .padding()
      HStack {
        Button("Request Restart") {
          _ = installation.notifyUserPromptedToRestart()
        }
        Button("Check Restart") {
          _ = installation.validateUserHasRestarted()
        }
        Button("debug") {
          installation.debug()
        }
        Button("Disable Keyman") {
          _ = installation.disableKeymanInputMethod()
        }
        Button("Kill Keyman") {
          _ = installation.killKeymanInputMethod()
        }
        Button("Uninstall") {
          installation.uninstall()
        }
        Button("Force Reset Installation") {
          installation.forceResetInstallation()
        }
        Button("Force Validate Installation") {
          installation.forceValidateInstallation()
        }
        Spacer()
      }
      .padding()
    }
    .padding()
//    .onReceive(NotificationCenter.default.publisher(for: .inputMethodMissing), perform: {_ in print("input method missing")})
  }
}

#Preview {
  let installation = InstallationContainer()
  InstallView()
    .environmentObject(installation)
}
