//
//  CompletedInstallView.swift
//  Config
//
//  Created by Eli Schantz on 7/1/26.
//

import SwiftUI

struct CompletedInstallView: View {
  @Binding var currentPage: NewInstallView.InstallPage
  @EnvironmentObject var installation: InstallationContainer
  @Environment(\.dismiss) private var dismiss
  let namespace: Namespace.ID
  
  var versionText: String {
      if let version = installation.installationState?.keymanVersion {
          return "Version: \(version)              "
      } else {
          return "No version to display"
      }
  }
  
  var body: some View {
    
    VStack {
      
      Spacer()
      
      KeymanLogo(namespace: namespace)
      
      Text(versionText)
        .foregroundStyle(.secondary)
      
      Spacer()
      
      GradientDivider(namespace: namespace)
        .padding(.bottom, 8)
      
      HStack {
        Text("Installation complete")
          .font(.title2)

          .frame(maxWidth: .infinity, alignment: .leading)
        
        ContinueButton(currentPage: $currentPage, nextPage: .initial)
      }
    }
  }
}

