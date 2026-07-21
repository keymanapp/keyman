//
//  EnableInputMethodView.swift
//  Config
//
//  Created by Eli Schantz on 7/1/26.
//

import SwiftUI

struct EnableInputMethodView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  
  var body: some View {
    
    VStack {
      
      Text("Enable Keyman")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      
      GradientDivider(namespace: namespace)
      
      Spacer()
      
      Image("EnableKeyman")
        .interpolation(.high)
        .resizable()
        .scaledToFit()
        .frame(height: 150)
        .padding(.horizontal, 100)
        .padding(.bottom, 8)
      
      Text("To use Keyman, enable the Keyman input method in System Settings.")
        .multilineTextAlignment(.center)
      
      
      Spacer()
      
      HStack {
        
        Text("Enable input method")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        
        
        Button {
            withAnimation(.smooth(duration: 0.5)) {
              _ = installation.enableKeymanInputMethod()
            }
        } label: {
            Text("Enable")
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
