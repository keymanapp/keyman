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

struct VisualEffectBlur: NSViewRepresentable {
    func makeNSView(context: Context) -> NSVisualEffectView {
        let view = NSVisualEffectView()
        view.material = .hudWindow      // Matches native dark/light HUD styling
        view.blendingMode = .withinWindow
        view.state = .active
        return view
    }
    
    func updateNSView(_ nsView: NSVisualEffectView, context: Context) {}
}

struct AddKeyboardView: View {
  @EnvironmentObject var settings: SettingsContainer
  @Environment(\.dismiss) private var dismissAddKeyboardView
  @StateObject private var downloadCoordinator = DownloadCoordinator()
  
  var body: some View {
    ZStack {
      KeyboardSearchView(coordinator: downloadCoordinator)
        .environmentObject(settings)
        .padding()
      
      if downloadCoordinator.isDownloading {
        // Dim the background slightly to focus on the progress panel
        Color.black.opacity(0.2)
          .transition(.opacity)
        
        VStack(spacing: 16) {
          Text("Downloading File...")
            .font(.headline)
          
          // Native progress bar bound to the coordinator's value (0.0 to 1.0)
          ProgressView(value: downloadCoordinator.downloadProgress, total: 1.0)
            .progressViewStyle(.linear)
            .frame(width: 250)
          
          Text("\(Int(downloadCoordinator.downloadProgress * 100))%")
            .font(.body)
            .foregroundColor(.secondary)
        }
        .padding(24)
        // Gives it a beautiful native translucent macOS look
        .background(VisualEffectBlur())
        .cornerRadius(12)
        .shadow(radius: 10)
        .transition(.scale.combined(with: .opacity))
      }
    }
    .animation(.default, value: downloadCoordinator.isDownloading)
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
