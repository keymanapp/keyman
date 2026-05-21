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
        Text("Installation State = \(installation.state)")
      }
      HStack {
        Button("Next...") {
          installation.executeNextInstallationStep()
        }
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
          installation.checkAccessibilityPermission()
        }
        Button("Request Permission") {
          _ = installation.requestAccessibilityPermission()
        }
        Spacer()
      }
      .padding()
      HStack {
        Button("Request Restart") {
          _ = installation.promptUserToRestart()
        }
        Button("Check Restart") {
          _ = installation.validateRestarted()
        }
        Button("debug") {
          installation.debug()
        }
        Button("Disable Keyman") {
          _ = installation.disableKeymanInputMethod()
        }
        Button("Run Keyman") {
          _ = installation.runKeymanInputMethod()
        }
        Button("Kill Keyman") {
          _ = installation.killKeymanInputMethod()
        }
        Button("Uninstall") {
          installation.uninstall()
        }
        Button("Reset Installation") {
          installation.resetInstallation()
        }
        Spacer()
      }
      .padding()
    }
    .padding()
  }
}

#Preview {
  let installation = InstallationContainer()
  InstallView()
    .environmentObject(installation)
}
