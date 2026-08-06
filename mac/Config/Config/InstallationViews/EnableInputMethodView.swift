/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-01
 *
 * View used for directing the user to enable the Keyman input method.
 */

import SwiftUI

struct EnableInputMethodView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  let onContinue: () -> Void
  @State var enableButtonPressed : Bool = false
  
  var body: some View {
    VStack {
      Text("Enable Keyman")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      GradientDivider(namespace: namespace)
      
      Color.clear
        .frame(height: 25)
      
      Form {
        Section {
          HStack {
            Spacer()
            Image("EnableKeyman")
              .interpolation(.high)
              .resizable()
              .scaledToFit()
              .frame(maxHeight: 200)
            Spacer()
          }
          Text("To use Keyman, enable the Keyman input method in System Settings.")
            .lineSpacing(6)
            .foregroundStyle(.secondary)
        }
      }
      .formStyle(.grouped)
            
      HStack {
        
        Spacer()
        
        Button {
          enableButtonPressed = true
          installation.executeCurrentInstallationTask()
        } label: {
          Text("Enable")
            .padding(.horizontal, 16)
            .padding(.vertical, 4)
        }
        .buttonStyle(.borderedProminent)
        .tint(.blue)
        .clipShape(Capsule())
        .matchedGeometryEffect(id: "actionButton", in: namespace)
        NavigationButton(action: .advance, onContinue: onContinue)
          .disabled(!enableButtonPressed)
      }
    }
  }
}
