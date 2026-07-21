//
//  AllowSecurityPermissionView.swift
//  Config
//
//  Created by Eli Schantz on 7/3/26.
//

import SwiftUI

struct AllowSecurityPermissionView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  
  var body: some View {
    VStack {
      
      Text("Allow Security Permission")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      
      GradientDivider(namespace: namespace)
      
      Spacer()
      
      
      Image("SecurityPermission")
        .interpolation(.high)
        .resizable()
        .scaledToFit()
        .frame(height: 130)
        .padding(.bottom, 8)
      
      Text("Toggle Keyman.app to provide it with necessary control in System Settings > Privacy & Security > Accessibility.")
        .multilineTextAlignment(.center)
      
      Spacer()
      
      HStack {
        
        Text("Toggle accessibility control")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        
        
        Button {
            withAnimation(.smooth(duration: 0.5)) {
              _ = installation.requestAccessibility()
            }
        } label: {
            Text("Allow")
                .padding(.horizontal, 16)
                .padding(.vertical, 4)
        }
        .buttonStyle(.borderedProminent)
        .tint(.blue)
        .clipShape(Capsule())

        ContinueButton()
      }
    }
  }
}
