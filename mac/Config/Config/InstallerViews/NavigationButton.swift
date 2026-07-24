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
  var action: ButtonAction
  
    var body: some View {
      
        Button {
          
          switch action {
          case .advance:
            NotificationCenter.default.post(name: Notification.Name("advancePage"), object: nil)
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

extension Notification.Name {
    static let advancePage = Notification.Name("advancePage")
}
