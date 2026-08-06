/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-01
 *
 * View used for providing an opening screen after the installation package has been run
 */

import SwiftUI
import Foundation


struct InitialInstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  
  let namespace: Namespace.ID
  let onContinue: () -> Void
  
  var versionText: String {
    if let version = installation.installationState?.keymanVersion {
      return "Version: \(version)              "
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
        Text("Proceed to continue with installation")
          .font(.title2)
          .multilineTextAlignment(.center)
          .frame(maxWidth: .infinity, alignment: .leading)
        NavigationButton(action: .advance, onContinue: onContinue)
      }
    }
  }
}

struct InitialInstallView_Previews: PreviewProvider {
  @Namespace static var namespace
  
  static var previews: some View {
    InitialInstallView(
      namespace: namespace,
      onContinue: { }
    )
    .environmentObject(InstallationContainer())
  }
}
