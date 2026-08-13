/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-21
 *
 * View used for directing the user to rerun the installer.
 * This is in the case that the input method needs to be restored.
 */

import SwiftUI
import AppKit
internal import UniformTypeIdentifiers

struct RerunInstallerView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  
  var body: some View {
    VStack {
      Text("Missing Keyman Components")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      GradientDivider(namespace: namespace)
      
      Form {
        HStack {
          Spacer()
          Image("InstallerIcon")
            .interpolation(.high)
            .resizable()
            .scaledToFit()
            .frame(height: 130)
            .padding(.bottom, 8)
          Spacer()
        }
        Text("Your Keyman input method is either missing or outdated. Run the Keyman installer to install a new version.")
          .multilineTextAlignment(.center)
          .foregroundStyle(.secondary)
      }
      .formStyle(.grouped)
      
      HStack {
        Text("Run Keyman installer")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        NavigationButton(action: .dismiss)
      }
    }
  }
}
