/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-06-16
 *
 * Contains webview to search for keyboards
 * Injects DownloadCoordinator to bridge back to SwiftUI
 */

import SwiftUI
import KeymanSettings
import OSLog

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
          ProgressView(value: downloadCoordinator.downloadProgressFraction, total: 1.0)
            .progressViewStyle(.linear)
            .frame(width: 250)
          
//          Text("\(Int(downloadCoordinator.downloadProgressFraction * 100))%")
          Text(downloadCoordinator.downloadProgressFraction, format: .percent.precision(.fractionLength(0)))
            .font(.body)
            .foregroundColor(.secondary)
        }
        .padding(24)
        // translucent macOS look
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
          Logger.app.debug("AddKeyboardView close button clicked")
          dismissAddKeyboardView()
          if settings.isInstallationInProgress() {
            settings.userCanceledPackageInstallation()
          }
        }
      }
    }
    .onDisappear {
      Logger.app.debug("AddKeyboardView onDisappear")
      downloadCoordinator.cancelActiveDownload()
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
            Logger.download.info("installing validated package: \(helper.packageName ?? "unknown package", privacy: .public)")
            LogUtil.infoBreadcrumb("installing validated package: \(helper.packageName ?? "unknown package")", category: .download)
            do {
              try settings.installPackage()
            } catch {
              Logger.download.error("failed to install package: \(helper.packageName ?? "unknown package") with error: \(error as NSError, privacy: .public)")
              LogUtil.errorBreadcrumb("failed to install package: \(helper.packageName ?? "unknown package") with error: \(error as NSError)", category: .download)
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

struct VisualEffectBlur: NSViewRepresentable {
  func makeNSView(context: Context) -> NSVisualEffectView {
    let view = NSVisualEffectView()
    view.material = .hudWindow      // matches native dark/light HUD styling
    view.blendingMode = .withinWindow
    view.state = .active
    return view
  }
  
  func updateNSView(_ nsView: NSVisualEffectView, context: Context) {}
}

