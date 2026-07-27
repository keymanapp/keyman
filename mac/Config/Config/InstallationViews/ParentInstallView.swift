/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 */

import SwiftUI

struct ParentInstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  @Namespace var animation
  @State public var currentPage: InstallPage = .loading
  
  func chooseCurrentPage() {
    if installation.installationPhase.hasTasks {
      switch installation.currentTask()?.taskType {
      case .prepareNewInstall: currentPage = .initial
      case .enableInputMethod:
        currentPage = .enableInputMethod
      case .requestAccess: currentPage =
          .allowSecurityPermission
      case .confirmAccess: print("Access granted")
      case .restartMac: currentPage = .restartComputer
      default: currentPage = .completed
      }
    } else {
      switch installation.installationPhase {
      case .evaluatingInstallation: currentPage = .loading
      case .inputMethodMissing, .inputMethodOutdated: currentPage = .rerunInstaller
      case .installationComplete: currentPage = .completed
      default: currentPage = .completed
      }
    }
    print("currentPage is now \(currentPage)")
  }
  
  var body: some View {
    ZStack {
      switch currentPage {
      case .loading: ProgressView()
      case .initial: InitialView(namespace: animation,onContinue: {
        installation.executeNextInstallationTask()
        chooseCurrentPage()
      })
      case .completed: CompletedInstallView(namespace: animation)
      case .enableInputMethod: EnableInputMethodView(namespace: animation, onContinue: chooseCurrentPage)
      case .allowSecurityPermission: GrantAccessibiltyPermissionView(namespace: animation, onContinue: chooseCurrentPage)
      case .rerunInstaller: RerunInstallerView(namespace: animation)
      case .restartComputer: RestartComputerView(namespace: animation)
      }
    }
    .onAppear {
      if installation.installationPhase == .evaluatingInstallation {
        currentPage = .loading
        Task {
          while installation.installationPhase == .evaluatingInstallation {
            try? await Task.sleep(for: .milliseconds(200))
          }
          await MainActor.run {
            withAnimation(.smooth) {
              chooseCurrentPage()
            }
          }
        }
      } else {
        chooseCurrentPage()
      }
    }
    .padding()
    .frame(minWidth: 600)
    .frame(minHeight: 400)
  }
}

#Preview {
  let installation = InstallationContainer()
  ParentInstallView()
    .environmentObject(installation)
}
