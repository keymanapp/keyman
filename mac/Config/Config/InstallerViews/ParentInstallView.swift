//
//  ViewController.swift
//  Config
//
//  Created by Eli Schantz on 6/29/26.
//

import SwiftUI

enum InstallPage: String, CaseIterable {
  case initial = "Install View"
  case completed = "Completed View"
  case enableInputMethod = "Enable Input Method View"
  case allowSecurityPermission = "Allow Security Permission View"
}

struct ParentInstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  @Namespace var animation
  @State public var currentPage: InstallPage = .initial
  @State var notifiedViews: [InstallPage] = [.initial, .enableInputMethod, .allowSecurityPermission, .completed]
  
  
  var body: some View {
    
    
    
    ZStack {
      switch currentPage {
      case .initial: InitialView(notifiedViews: $notifiedViews, namespace: animation, )
      case .completed: CompletedInstallView(namespace: animation)
      case .enableInputMethod: EnableInputMethodView(namespace: animation)
      case .allowSecurityPermission: AllowSecurityPermissionView(namespace: animation)
      }
      
    }
    .onReceive(NotificationCenter.default.publisher(for: .advancePage)) { notification in
          withAnimation(.smooth(duration: 0.5)) {
            if let currentIndex = notifiedViews.firstIndex(of: currentPage) {
              let nextIndex = currentIndex + 1
              if nextIndex < notifiedViews.count {
                currentPage = notifiedViews[nextIndex]
              } else {
                currentPage = .initial
              }
            }
          }
        }
    .padding()
    .frame(width: 500)
    .frame(height: 400)
  }
  
}



#Preview {
  let installation = InstallationContainer()
    ParentInstallView()
    .environmentObject(installation)
}
