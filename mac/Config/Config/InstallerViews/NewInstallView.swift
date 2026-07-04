//
//  NewInstall.swift
//  Config
//
//  Created by Eli Schantz on 6/29/26.
//

import SwiftUI

struct NewInstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  @Namespace var animation
  
  enum InstallPage {
    case initial
    case completed
    case enableInputMethod
    case allowSecurityPermission
  }
  
  @State public var currentPage: InstallPage = .initial
  
  var body: some View {
    
    
    
    
    Group {
      switch currentPage {
      case .initial:
        InitialInstallView(currentPage: $currentPage, namespace: animation)
      case .completed:
        CompletedInstallView(currentPage: $currentPage, namespace: animation)
      case .enableInputMethod:
        EnableInputMethodView(currentPage: $currentPage, namespace: animation)
    
      case .allowSecurityPermission:
        AllowSecurityPermissionView(currentPage: $currentPage, namespace: animation)
      }
    }
    .padding()
    .frame(width: 500,)
    .frame(height: 400)
  }
  
}

#Preview {
  let installation = InstallationContainer()
  NewInstallView()
    .environmentObject(installation)
}
