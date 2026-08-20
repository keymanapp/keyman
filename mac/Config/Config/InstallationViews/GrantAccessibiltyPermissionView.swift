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
  
  /**
   * The flow of this view depends on the following @State variables.
   * Once the user presses "Open Settings" in order to toggle the security permission they will be allowed to continue.
   * When they press "Continue," a loading symbol will run until the view receives a notification of whether the access is
   * granted or not. If not granted, an error message will appear. If granted, the user will be moved to the next screen.
   */
  
  // Tracks if the user clicked "Open Settings" (Enables the "Continue" button)
  @State var openSettingsButtonPressed: Bool = false
  // Tracks if the app is currently running the background permission check
  @State var checkingPermission: Bool = false
  // Tracks if the user clicked "Continue" but permission is still missing
  @State var permissionNotGrantedAfterPrompt: Bool = false
  
  var body: some View {
    VStack {
      Text("Grant Accessibility Permission")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      GradientDivider(namespace: namespace)
      
      Form {
        Section {
          Image("accessibility-permission")
            .interpolation(.high)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .padding(.bottom, 8)
          VStack(alignment: .leading, spacing: 8) {
            Text("Ensure Keyman.app is set to provide it with necessary control in System Settings > Privacy & Security > Accessibility.")
              .lineSpacing(6)
              .lineLimit(2)
              .fixedSize(horizontal: false, vertical: true) // prevents vertical compression
              .foregroundStyle(.secondary)
          }
        }
      }
      .formStyle(.grouped)
      .frame(maxHeight: .infinity, alignment: .center)
      .padding(.top, 10)
      
      HStack {
        
        Spacer()
        
        if checkingPermission {
          HStack {
            // Shows spinner AKA ProgressView()
            ProgressView()
              .controlSize(.small)
            
            Text("Checking...")
          }
        } else if permissionNotGrantedAfterPrompt {
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
          // Trigger the system task to check for accessibility permission
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
    // Triggered when the system confirms accessibility has been granted.
    .onReceive( NotificationCenter.default.publisher(for: .checkAccessibilitySuccess)) { notification in
      withAnimation(.smooth) {
        permissionNotGrantedAfterPrompt = false
        onContinue() // Moves the user to the next screen
      }
    }
    // Triggered when the system confirms accessibility has not been granted.
    .onReceive( NotificationCenter.default.publisher(for: .checkAccessibilityFailure)) { notification in
      withAnimation(.smooth) {
        checkingPermission = false // Stops showing the loading spinner
        permissionNotGrantedAfterPrompt = true // Shows the red error text
      }
    }
  }
}
