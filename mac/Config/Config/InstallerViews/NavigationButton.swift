//
//  NavigationButton.swift
//  Config
//
//  Created by Eli Schantz on 7/2/26.
//

import SwiftUI
import Combine

enum ButtonAction {
  case advance
  case dismiss
}

struct NavigationButton: View {
  @Environment(\.dismiss) private var dismiss
  @EnvironmentObject var installation: InstallationContainer
  var action: ButtonAction = .advance
  var onContinue: () -> Void = {}
  
    var body: some View {
      
        Button {
          
          switch action {
          case .advance:
            installation.executeNextInstallationTask()
            onContinue()
          case .dismiss:
            dismiss()
          }
          
          
        } label: {
          
            switch action {
              
            case .dismiss:
              Text("Close")
                .padding(.horizontal, 16)
                .padding(.vertical, 4)
              
            default:
              Text("Continue")
                .padding(.horizontal, 16)
                .padding(.vertical, 4)
          }
            
        }
        .clipShape(Capsule())
    }
}
