//
//  RerunInstallerView.swift
//  Config
//
//  Created by Eli Schantz on 7/21/26.
//

import SwiftUI
import AppKit
internal import UniformTypeIdentifiers

struct RerunInstallerView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  
  var body: some View {
    VStack {
      
      Text("Missing Keyman Components")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      
      GradientDivider(namespace: namespace)
      
      Spacer()
      
      Image("InstallerIcon")
        .interpolation(.high)
        .resizable()
        .scaledToFit()
        .frame(height: 130)
        .padding(.bottom, 8)
      
      Text("Your Keyman input method is either missing or outdated. Run the Keyman installer to install a new version.")
              .multilineTextAlignment(.center)
      
      Spacer()
      
      HStack {
        
        Text("Run Keyman installer")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        
          .buttonStyle(.borderedProminent)
          .tint(.blue)
          .clipShape(Capsule())
        
        Button {
          chooseAndOpenKeymanInstaller()
        } label: {
          Text("Open Installer")
            .padding(.horizontal, 16)
            .padding(.vertical, 4)
        }
        .buttonStyle(.borderedProminent)
        .tint(.blue)
        .clipShape(Capsule())
        .matchedGeometryEffect(id: "actionButton", in: namespace)
      }
    }
  }
}

private func chooseAndOpenKeymanInstaller() {
  let panel = NSOpenPanel()
  panel.message = "Open the Keyman .pkg file."
  panel.allowedContentTypes = [UTType(filenameExtension: "pkg")!]
  panel.directoryURL = FileManager.default.urls(
      for: .downloadsDirectory,
      in: .userDomainMask
  ).first

  if panel.runModal() == .OK, let url = panel.url {
      NSWorkspace.shared.open(url)
  }
}
