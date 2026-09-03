/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * Test view used for configuring Keyman
 * Not included in ConfigApp.swift, but can be added temporarily for testing purposes
 * All text and labels are marked verbatim, so that they do not get extracted for localization
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
        Text(verbatim: "Current task = \(taskText)")
          .onAppear() {
            if let installTask = installation.currentTask() {
              taskText = installTask.taskType.rawValue
            }
          }
      }
      HStack {
        Button {
          installation.executeCurrentInstallationTask()
          if let installTask = installation.currentTask() {
            taskText = installTask.taskType.rawValue
          }
        } label: {
          Text(verbatim: "Next...")
        }
        .disabled(installation.isInstallationComplete())
        Button {
          _ = installation.migrateData()
        } label: {
          Text(verbatim: "Migrate Data")
        }
        Button {
          _ = installation.registerKeymanInputMethod()
        } label: {
          Text(verbatim: "Register Keyman")
        }
        Button {
          _ = installation.enableKeymanInputMethod()
        } label: {
          Text(verbatim: "Enable Keyman")
        }
        Button {
          _ = installation.selectKeymanInputMethod()
        } label: {
          Text(verbatim: "Select Keyman")
        }
        Button {
          installation.checkAccessibilityPermissionGranted()
        } label: {
          Text(verbatim: "Check Permission")
        }
        Button {
          _ = installation.requestAccessibility()
        } label: {
          Text(verbatim: "Request Permission")
        }
        Spacer()
      }
      .padding()
      HStack {
        Button {
          _ = installation.notifyUserPromptedToRestart()
        } label: {
          Text(verbatim: "Request Restart")
        }
        Button {
          _ = installation.validateUserHasRestarted()
        } label: {
          Text(verbatim: "Check Restart")
        }
        Button {
          installation.setHasDisplayedInstallationComplete()
        } label: {
          Text(verbatim: "Set Displayed Complete")
        }
        Button {
          installation.debug()
        } label: {
          Text(verbatim: "debug")
        }
        Button {
          _ = installation.disableKeymanInputMethod()
        } label: {
          Text(verbatim: "Disable Keyman")
        }
        Button {
          _ = installation.killKeymanInputMethod()
        } label: {
          Text(verbatim: "Kill Keyman")
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
