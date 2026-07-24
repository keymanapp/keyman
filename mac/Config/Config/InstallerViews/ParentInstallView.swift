//
//  ParentInstallView.swift
//  Config
//
//  Created by Eli Schantz on 6/29/26.
//

import SwiftUI

struct ParentInstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  @Namespace var animation
  @State public var currentPage: InstallPage = .initial
  
  var body: some View {
    
    
    
    ZStack {
      switch currentPage {
      case .initial: InitialView(namespace: animation, )
      case .completed: CompletedInstallView(namespace: animation)
      case .enableInputMethod: EnableInputMethodView(namespace: animation)
      case .allowSecurityPermission: CheckAccessibiltyPermissionView(namespace: animation)
      case .rerunInstaller:
        RerunInstallerView(namespace: animation)
        
      case .restartComputer:
        RestartComputerView(namespace: animation)
      }
      
      
    }
    .onReceive(NotificationCenter.default.publisher(for: .advancePage)) { notification in
          withAnimation(.smooth(duration: 0.5)) {
          }
        }
    .onAppear {
      switch installation.installationPhase {
      case .evaluatingInstallation:
        currentPage = .initial
      default:
        currentPage = .completed
      }
    }
    
    .padding()
    .frame(width: 600)
    .frame(height: 400)
  }
  
}

#Preview {
  let installation = InstallationContainer()
  ParentInstallView()
    .environmentObject(installation)
}
