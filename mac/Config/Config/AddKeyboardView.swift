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

struct AddKeyboardView: View {
  @EnvironmentObject var settings: SettingsContainer
  @Environment(\.dismiss) private var dismissAddKeyboardView
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
          dismissAddKeyboardView()
        }
      }
    }
    .alert("Package Installation Failed", isPresented: $downloadCoordinator.loadPackageFailed) {
        Button("OK", role: .cancel) { }
    } message: {
      if let message = downloadCoordinator.loadFailureMessage {
        Text(message)
      }
    }
    .sheet(isPresented: $downloadCoordinator.showConfirmPackageSheet) {
      if let helper = downloadCoordinator.installHelper {
        PackageConfirmationView(installHelper: helper) { accepted in
          if accepted {
            print("installing validated package: \(helper.packageName ?? "unknown package")")
            do {
              try settings.installPackage()
            } catch {
              print("failed to install package: \(helper.packageName ?? "unknown package") with error: \(error.localizedDescription)")
            }
          } else {
            settings.userCanceledPackageInstallation()
          }
          
          // close sheet
          downloadCoordinator.showConfirmPackageSheet = false
          // close
          dismissAddKeyboardView()
        }
      }
    }
  }
}
