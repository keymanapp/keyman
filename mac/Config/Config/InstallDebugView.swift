/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * Main view used for configuring Keyman
 */

import SwiftUI

struct InstallDebugView: View {
  @EnvironmentObject var installation: InstallationContainer
  @State private var taskText: String = "[task]"

  var body: some View {
    VStack {
      HStack {
        Image(systemName: "gear")
          .imageScale(.large)
          .foregroundColor(.accentColor)
        Text("Current task = \(taskText)")
          .onAppear() {
            if let installTask = installation.currentTask() {
              taskText = installTask.taskType.rawValue
            }
          }
      }
      HStack {
        Button("Next...") {
          installation.executeCurrentInstallationTask()
          if let installTask = installation.currentTask() {
            taskText = installTask.taskType.rawValue
          }
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
        Button("Set Displayed Complete") {
          let beforeDisplayed = installation.getHasDisplayedInstallationComplete()
          installation.setHasDisplayedInstallationComplete()
          let afterDisplayed = installation.getHasDisplayedInstallationComplete()
          print("hasDisplayedInstallComplete = \(beforeDisplayed) -> \(afterDisplayed)")
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
  InstallDebugView()
    .environmentObject(installation)
}
