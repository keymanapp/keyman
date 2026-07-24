//
//  InitialView.swift
//  Config
//
//  Created by Eli Schantz on 7/1/26.
//

import SwiftUI
import Foundation


struct InitialView: View {
  
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
        Text("Proceed to continue with installation")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        
        NavigationButton(action: .advance)
      }
    }
  }
}
