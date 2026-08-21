/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-27
 *
 * View used for providing an opening screen for the repair phase.
 */

import SwiftUI
import AppKit
internal import UniformTypeIdentifiers

struct InitialRepairView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  let onContinue: () -> Void
  
  var body: some View {
    VStack {
//      Label("Repairs Required", systemImage: "hand.raised.fill")
      Text("Repairs Required")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      GradientDivider(namespace: namespace)
      
      Form {
        HStack {
          Spacer()
          Image(systemName: "wrench.and.screwdriver.fill")
            .font(.system(size: 100))
            .symbolRenderingMode(.palette)
            .foregroundStyle(
                Color("Keyman Blue"),     // first color for the wrench
                Color("Keyman Orange")    // second color for the screwdriver
            )
            .padding(.bottom, 16)
          Spacer()
        }
        Text("One or more Keyman components or permissions require your attention. Complete the following steps to restore your Keyman installation.")
          .multilineTextAlignment(.leading)
      }
      .formStyle(.grouped)
      .padding(.top, 50)

      HStack {
        Text("Resolve Issues")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        NavigationButton(action: .advance, onContinue: onContinue)
          .buttonStyle(.borderedProminent)
          .tint(.blue)
          .clipShape(Capsule())
      }
    }
  }
}
