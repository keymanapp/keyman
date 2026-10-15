/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-01
 *
 * View used for notifying the user that installation is complete.
 */

import SwiftUI

struct CompletedInstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  
  var versionText: String {
    if let version = installation.installationState?.keymanVersion {
      return "Version: \(version)"
    } else {
      return "No version to display"
    }
  }
  
  var body: some View {
    VStack {
      
      Spacer()
      
      KeymanLogo(namespace: namespace)
      Text(versionText)
        .foregroundStyle(.secondary)
      
      Spacer()
      
      GradientDivider(namespace: namespace)
        .padding(.bottom, 8)
      HStack {
        Text("Installation complete")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        NavigationButton(action: .dismiss)
      }
    }
  }
}
