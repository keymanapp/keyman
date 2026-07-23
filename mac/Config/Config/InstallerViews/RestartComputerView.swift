//
//  RestartComputerView.swift
//  Config
//
//  Created by Eli Schantz on 7/21/26.
//

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
      
      GradientDivider(namespace: namespace)
      
      Spacer()
      
      
      Image(systemName: "restart.circle.fill")
        .font(.system(size: 88))
        .padding(8)
      
      Text("A restart is required for the previous installation steps to take effect.")
        .multilineTextAlignment(.center)
      
      Spacer()
      
      HStack {
        
        Text("Finish installation")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        
        
        
        Button {
          restart()
          
        } label: {
            Text("Restart Now")
                .padding(.horizontal, 16)
                .padding(.vertical, 4)
        }
        .buttonStyle(.borderedProminent)
        .tint(.blue)
        .clipShape(Capsule())
        .matchedGeometryEffect(id: "actionButton", in: namespace)
        
        NavigationButton(action: .dismiss)
      }
    }
  }
}


func restart() {
    let script = """
    tell application "System Events"
        restart
    end tell
    """

    NSAppleScript(source: script)?.executeAndReturnError(nil)
}
