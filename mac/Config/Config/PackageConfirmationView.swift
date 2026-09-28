/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-16
 *
 * View presented as modal sheet in response to initiating a package installation.
 * Displays readme.htm contents and allows user to proceed with install or cancel.
 */

import SwiftUI
import KeymanSettings

struct PackageConfirmationView: View {
  let installHelper: PackageInstallHelper
  let completion: (Bool) -> Void
  
  var body: some View {
    VStack(spacing: 16) {
      if let installationPrompt = installHelper.packageInstallationType?.prompt {
        let packageName = installHelper.packageToInstall?.packageName ?? "Unknown"
        Label(packageName, systemImage: "keyboard")
          .font(.title)
          .foregroundStyle(Color("Keyman Orange"))
          .bold()
          .frame(maxWidth: .infinity, alignment: .center)
        Text(installationPrompt)
          .font(.title3)
          .multilineTextAlignment(.leading)
      }
      
      if let readmeFileUrl = installHelper.packageToInstall?.readmeFileUrl {
        PackageContentWebView(packageFileUrl: readmeFileUrl)
          .cornerRadius(8)
          .padding(6)
          .overlay(
            RoundedRectangle(cornerRadius: 14)
              .stroke(Color("Keyman Orange"), lineWidth: 2)
          )
          .padding()
      } else {
        Text("Read me not available.")
          .font(.title)
      }
      
      HStack {
        Button("Cancel") {
          completion(false)
        }
        .keyboardShortcut(.cancelAction)
        
        Button("Install") {
          completion(true)
        }
        .buttonStyle(.borderedProminent)
      }
    }
    .padding()
    .frame(width: 580, height: 500)
  }
}
