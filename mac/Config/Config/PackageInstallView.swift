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

struct PackageInstallView: View {
  let installHelper: PackageInstallHelper
  let completion: (Bool) -> Void
  
  var body: some View {
    VStack(spacing: 20) {
      if let installationPrompt = installHelper.packageInstallationType?.prompt {
        Text(installationPrompt)
          .font(.title2)
          .multilineTextAlignment(.leading)
      }

      if let readmeFileUrl = installHelper.packageToInstall?.readmeFileUrl {
        PackageContentWebView(packageFileUrl: readmeFileUrl)
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
    .frame(width: 540, height: 400)
  }
}
