//
//  CheckAccessiblityPermissionView.swift
//  Config
//
//  Created by Eli Schantz on 7/3/26.
//

import SwiftUI
import AppKit

func openAccessibilitySettings() {
    if let url = URL(string: "x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility") {
        NSWorkspace.shared.open(url)
    }
}


struct CheckAccessibiltyPermissionView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  
  var body: some View {
    VStack {
      
      Text("Check Accessibilty Permission")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      
      GradientDivider(namespace: namespace)
      
      Spacer()
      
      
      Image("AccessibilityPermission")
        .interpolation(.high)
        .resizable()
        .scaledToFit()
        .frame(height: 130)
        .padding(.bottom, 8)
      
      Text("Ensure Keyman.app is toggled to provide it with necessary control in System Settings > Privacy & Security > Accessibility.")
        .multilineTextAlignment(.center)
      
      Spacer()
      
      HStack {
        
        Text("Check accessibility control")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        
        
        Button {
            _ = installation.requestAccessibility()
          
        } label: {
          Text("Open Settings")
                .padding(.horizontal, 16)
                .padding(.vertical, 4)
        }
        .buttonStyle(.borderedProminent)
        .tint(.blue)
        .clipShape(Capsule())
        .matchedGeometryEffect(id: "actionButton", in: namespace)

        NavigationButton(action: .advance)
      }
    }
  }
}
