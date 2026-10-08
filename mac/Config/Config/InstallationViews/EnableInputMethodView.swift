/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-01
 *
 * View used for directing the user to enable the Keyman input method.
 */

import SwiftUI

func openKeyboardSettings() {
  if let url = URL(string: "x-apple.systempreferences:com.apple.Keyboard") {
    NSWorkspace.shared.open(url)
  }
}

struct EnableInputMethodView: View {
  @EnvironmentObject var installation: InstallationContainer
  @Environment(\.openWindow) private var openWindow
  
  let namespace: Namespace.ID
  let onContinue: () -> Void
  @State var enableButtonPressed : Bool = false

  // tracks whether the user has enabled the input method in the System Settings
  @State var inputMethodEnabled: Bool = false

  var body: some View {
    VStack {
      Text("Enable Keyman")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      GradientDivider(namespace: namespace)
      
      Form {
        Section {
          HStack {
            Spacer()
            Image("enable-keyman")
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
      .padding(.top, 25)
      
      HStack {
        
        Spacer()
        
        if inputMethodEnabled {
          Text("Input method has been enabled")
            .font(.title2)
            .frame(maxWidth: .infinity, alignment: .leading)
            .transition(
              .scale(scale: 0.1, anchor: .center)
              .combined(with: .opacity)
            )
        }
        
        Button {
          if !enableButtonPressed {
            installation.executeCurrentInstallationTask()
            enableButtonPressed = true
          } else {
            openKeyboardSettings()
          }
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
    // triggered when the system confirms that
    .onReceive( NotificationCenter.default.publisher(for: .inputMethodEnabled)) { notification in
      
      // bring the app to the front, just in case it is being blocked by the System Settings
      NSApplication.shared.activate(ignoringOtherApps: true)
      openWindow(id: "install")
      
      // wait 0.2 second for the window to switch before animating
      DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
        withAnimation(.spring(response: 0.45, dampingFraction: 0.55)) {
          inputMethodEnabled = true
        }
      }
      
      Task { @MainActor in
        // wait 1.25 seconds (1,250,000,000 nanoseconds)
        try? await Task.sleep(nanoseconds: 1_250_000_000)
        
        withAnimation(.smooth) {
          onContinue() // moves the user to the next screen
        }
      }
    }
  }
}
