/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-16
 *
 * Contains webview to search for keyboards and injects
 * DownloadCoordinator to bridge back to SwiftUI
 */

import SwiftUI
import KeymanSettings

struct InstallKeyboardView: View {
  @EnvironmentObject var settings: SettingsContainer
  @Environment(\.dismiss) private var dismiss
  @StateObject private var downloadCoordinator = DownloadCoordinator()
  
  var body: some View {
    VStack {
      KeyboardSearchView(coordinator: downloadCoordinator)
        .environmentObject(settings)
        .padding()
    }
    .toolbar {
      // Placement determines where on the bar it sits
      ToolbarItem(placement: .cancellationAction) {
        Button("Close") {
          dismiss()
        }
      }
    }
    .sheet(isPresented: $downloadCoordinator.showInstallSheet) {
      if let helper = downloadCoordinator.installHelper {
        PackageInstallView(installHelper: helper) { accepted in
          if accepted {
            print("Processing validated package: \(helper.packageName ?? "unknown package")")
            do {
              try settings.installPackage()
            } catch {
              print("failed to install package: \(helper.packageName ?? "unknown package") with error: \(error.localizedDescription)")
            }
          } else {
            settings.userCanceledPackageInstallation()
          }
          downloadCoordinator.showInstallSheet = false
          dismiss()
        }
      }
    }
  }
}
