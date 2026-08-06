/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-03
 *
 * View used for directing the user to grant Keyman accessibility permission.
 */

import SwiftUI
import AppKit

func openAccessibilitySettings() {
  if let url = URL(string: "x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility") {
    NSWorkspace.shared.open(url)
  }
}

struct GrantAccessibiltyPermissionView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  let onContinue: () -> Void
  
  @State var openSettingsButtonPressed: Bool = false
  @State var checkingPermission: Bool = false
  @State var advancementRequestedAndPermissionNotGranted: Bool = false
  
  var body: some View {
    VStack {
      Text("Grant Accessibilty Permission")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      GradientDivider(namespace: namespace)
      
      Color.clear
        .frame(height: 50)
        .hidden()
      
      Form {
        Section {
          Image("AccessibilityPermission")
            .interpolation(.high)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .padding(.bottom, 8)
          Text("Ensure Keyman.app is toggled to provide it with necessary control in System Settings > Privacy & Security > Accessibility.")
            .lineSpacing(6)
            .foregroundStyle(.secondary)
        }
      }
      .formStyle(.grouped)
      .frame(maxHeight: .infinity, alignment: .center)
      
      HStack {
        
        Spacer()
        
        if checkingPermission {
          HStack {
            ProgressView()
              .controlSize(.small)
            
            Text("Checking...")
          }
        } else if advancementRequestedAndPermissionNotGranted {
          Text("Access has not been granted.")
            .foregroundStyle(Color.red)
            .padding(7)
            .background(.thinMaterial)
            .clipShape(RoundedRectangle(cornerRadius: 12, style: .continuous))
        }

        
        Button {
          if installation.currentTask()?.taskType == .requestAccess {
            installation.executeCurrentInstallationTask()
            openSettingsButtonPressed = true
          } else {
            openAccessibilitySettings()
            openSettingsButtonPressed = true
          }
        } label: {
          Text("Open Settings")
            .padding(.horizontal, 16)
            .padding(.vertical, 4)
        }
        .buttonStyle(.borderedProminent)
        .tint(.blue)
        .clipShape(Capsule())
        .matchedGeometryEffect(id: "actionButton", in: namespace)
        Button {
          checkingPermission = true
          installation.executeCurrentInstallationTask()
        } label: {
          Text("Continue")
            .padding(.horizontal, 16)
            .padding(.vertical, 4)
        }
        .disabled(!openSettingsButtonPressed)
        .clipShape(Capsule())
      }
    }
    .onReceive( NotificationCenter.default.publisher(for: .accessibilityGranted)) { notification in
      withAnimation(.smooth) {
        advancementRequestedAndPermissionNotGranted = true
        onContinue()
      }
    }
    .onReceive( NotificationCenter.default.publisher(for: .accessibilityNotGranted)) { notification in
      withAnimation(.smooth) {
        checkingPermission = false
        advancementRequestedAndPermissionNotGranted = true
      }
    }
  }
}
