/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-21
 *
 * View used for directing the user to restart their mac.
 */

import SwiftUI

struct RestartComputerView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  
  var body: some View {
    VStack {
      Text("Restart Computer")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      
      Spacer()
      
      Image(systemName: "restart.circle.fill")
        .font(.system(size: 100))
        .padding(16)
      Text("Restart your Mac to complete the installation. After restarting, open Keyman Configuration again if it doesn't launch automatically.")
        .multilineTextAlignment(.center)
        .padding(.bottom, 8)
      
      Spacer()
      
      GradientDivider(namespace: namespace)
        .padding(.bottom, 8)
      HStack {
        Text("Finish installation")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        NavigationButton(action: .dismiss)
      }
    }
    .onAppear {
      installation.executeNextInstallationTask()
    }
  }
}
