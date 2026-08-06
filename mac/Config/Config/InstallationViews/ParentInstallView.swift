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
      case .prepareNewInstall: currentPage = .initialInstall
      case .prepareNewRepair: currentPage = .initialRepair
      case .enableInputMethod: currentPage = .enableInputMethod
      case .requestAccess: currentPage = .allowSecurityPermission
      case .confirmAccess: currentPage = .allowSecurityPermission
      default: currentPage = .completed
      }
    } else {
      switch installation.installationPhase {
      case .evaluatingInstallation: currentPage = .loading
      case .inputMethodMissing, .inputMethodOutdated: currentPage = .rerunInstaller
      case .installationComplete:
        currentPage = .completed
        if !installation.getHasDisplayedInstallationComplete() {
          installation.setHasDisplayedInstallationComplete()
        }
      default:
        currentPage = .completed
      }
    }
   
  }
  
  var body: some View {
    ZStack {
      switch currentPage {
      case .loading: ProgressView()
      case .initialInstall: InitialInstallView(namespace: animation,onContinue: {
        installation.executeCurrentInstallationTask()
        chooseCurrentPage()
      })
      case .initialRepair: InitialRepairView(namespace: animation,onContinue: {
        installation.executeCurrentInstallationTask()
        chooseCurrentPage()
      })
      case .completed: CompletedInstallView(namespace: animation)
      case .enableInputMethod: EnableInputMethodView(namespace: animation, onContinue: chooseCurrentPage)
      case .allowSecurityPermission: GrantAccessibiltyPermissionView(namespace: animation, onContinue: chooseCurrentPage)
      case .rerunInstaller: RerunInstallerView(namespace: animation)
      }
    }
    .onAppear {
      print("LOL ", installation.installationPhase)
      print("LOL ", installation.currentTask()?.taskType ?? "no task available")
      
      if installation.installationPhase == .evaluatingInstallation {
        currentPage = .loading
        Task {
          while installation.installationPhase == .evaluatingInstallation {
            try? await Task.sleep(for: .milliseconds(200))
          }
          await MainActor.run {
            withAnimation(.smooth) {
              chooseCurrentPage()
              print("LOOL ", installation.installationPhase)

            }
          }
        }
      } else {
        chooseCurrentPage()
      }
    }
    .padding()
    .frame(
      minWidth: 600, idealWidth: 600, maxWidth: 600,
      minHeight: 500, idealHeight: 500, maxHeight: 500
  )
  }
}
